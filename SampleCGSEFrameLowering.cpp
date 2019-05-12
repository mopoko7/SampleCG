//===- SampleCGSEFrameLowering.cpp - SampleCG32/64 Frame Information --------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the SampleCG32/64 implementation of TargetFrameLowering class.
//
//===----------------------------------------------------------------------===//

#include "SampleCGSEFrameLowering.h"
#include "MCTargetDesc/SampleCGABIInfo.h"
#include "SampleCGMachineFunction.h"
#include "SampleCGRegisterInfo.h"
#include "SampleCGSubtarget.h"
#include "SampleCGSEInstrInfo.h"
#include "llvm/ADT/BitVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineModuleInfo.h"
#include "llvm/CodeGen/MachineOperand.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/RegisterScavenging.h"
#include "llvm/CodeGen/TargetInstrInfo.h"
#include "llvm/CodeGen/TargetRegisterInfo.h"
#include "llvm/CodeGen/TargetSubtargetInfo.h"
#include "llvm/IR/DebugLoc.h"
#include "llvm/IR/Function.h"
#include "llvm/MC/MCDwarf.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MachineLocation.h"
#include "llvm/Support/CodeGen.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/MathExtras.h"
#include <cassert>
#include <cstdint>
#include <utility>
#include <vector>

using namespace llvm;

SampleCGSEFrameLowering::SampleCGSEFrameLowering(const SampleCGSubtarget &STI)
    : SampleCGFrameLowering(STI, STI.getStackAlignment()) {}

void SampleCGSEFrameLowering::emitPrologue(MachineFunction &MF,
                                       MachineBasicBlock &MBB) const {
  assert(&MF.front() == &MBB && "Shrink-wrapping not yet supported");
  MachineFrameInfo &MFI    = MF.getFrameInfo();
  SampleCGFunctionInfo *SampleCGFI = MF.getInfo<SampleCGFunctionInfo>();

  const SampleCGSEInstrInfo &TII =
    *static_cast<const SampleCGSEInstrInfo*>(STI.getInstrInfo());
  const SampleCGRegisterInfo &RegInfo =
      *static_cast<const SampleCGRegisterInfo *>(STI.getRegisterInfo());

  MachineBasicBlock::iterator MBBI = MBB.begin();
  DebugLoc dl = MBBI != MBB.end() ? MBBI->getDebugLoc() : DebugLoc();
  SampleCGABIInfo ABI = STI.getABI();
  unsigned SP = SampleCG::SP;
  unsigned FP = SampleCG::FP;
  unsigned ZERO = SampleCG::ZERO;
  unsigned ADDu = SampleCG::ADDu;
  const TargetRegisterClass *RC = &SampleCG::GPROutRegClass;

  // First, compute final stack size.
  uint64_t StackSize = MFI.getStackSize();

  // No need to allocate space on the stack.
  if (StackSize == 0 && !MFI.adjustsStack()) return;

  MachineModuleInfo &MMI = MF.getMMI();
  const MCRegisterInfo *MRI = MMI.getContext().getRegisterInfo();
  MachineLocation DstML, SrcML;

  // Adjust stack.
  TII.adjustStackPtr(SP, -StackSize, MBB, MBBI);

  // emit ".cfi_def_cfa_offset StackSize"
  unsigned CFIIndex = MF.addFrameInst(
      MCCFIInstruction::createDefCfaOffset(nullptr, -StackSize));
  BuildMI(MBB, MBBI, dl, TII.get(TargetOpcode::CFI_INSTRUCTION))
      .addCFIIndex(CFIIndex);

  const std::vector<CalleeSavedInfo> &CSI = MFI.getCalleeSavedInfo();

  if (CSI.size()) {
    // Find the instruction past the last instruction that saves a callee-saved
    // register to the stack.
    for (unsigned i = 0; i < CSI.size(); ++i)
      ++MBBI;

    // Iterate over list of callee-saved registers and emit .cfi_offset
    // directives.
    for (std::vector<CalleeSavedInfo>::const_iterator I = CSI.begin(),
           E = CSI.end(); I != E; ++I) {
      int64_t Offset = MFI.getObjectOffset(I->getFrameIdx());
      unsigned Reg = I->getReg();
      {
        // Reg is in CPURegs.
        unsigned CFIIndex = MF.addFrameInst(MCCFIInstruction::createOffset(
            nullptr, MRI->getDwarfRegNum(Reg, 1), Offset));
        BuildMI(MBB, MBBI, dl, TII.get(TargetOpcode::CFI_INSTRUCTION))
            .addCFIIndex(CFIIndex);
      }
    }
  }

  if (SampleCGFI->callsEhReturn()) {
    // Insert instructions that spill eh data registers.
    for (int I = 0; I < ABI.EhDataRegSize(); ++I) {
      if (!MBB.isLiveIn(ABI.GetEhDataReg(I)))
        MBB.addLiveIn(ABI.GetEhDataReg(I));
      TII.storeRegToStackSlot(MBB, MBBI, ABI.GetEhDataReg(I), false,
                              SampleCGFI->getEhDataRegFI(I), RC, &RegInfo);
    }

    // Emit .cfi_offset directives for eh data registers.
    for (int I = 0; I < ABI.EhDataRegSize(); ++I) {
      int64_t Offset = MFI.getObjectOffset(SampleCGFI->getEhDataRegFI(I));
      unsigned Reg = MRI->getDwarfRegNum(ABI.GetEhDataReg(I), true);
      unsigned CFIIndex = MF.addFrameInst(
          MCCFIInstruction::createOffset(nullptr, Reg, Offset));
      BuildMI(MBB, MBBI, dl, TII.get(TargetOpcode::CFI_INSTRUCTION))
          .addCFIIndex(CFIIndex);
    }
  }

  // if framepointer enabled, set it to point to the stack pointer.
  if (hasFP(MF)) {
    if (SampleCGFI->callsEhDwarf()) {
      BuildMI(MBB, MBBI, dl, TII.get(ADDu), SampleCG::V0).addReg(FP).addReg(ZERO)
        .setMIFlag(MachineInstr::FrameSetup);
    }
    //@ Insert instruction "move $fp, $sp" at this location.
    BuildMI(MBB, MBBI, dl, TII.get(ADDu), FP).addReg(SP).addReg(ZERO)
      .setMIFlag(MachineInstr::FrameSetup);

    // emit ".cfi_def_cfa_register $fp"
    unsigned CFIIndex = MF.addFrameInst(MCCFIInstruction::createDefCfaRegister(
        nullptr, MRI->getDwarfRegNum(FP, true)));
    BuildMI(MBB, MBBI, dl, TII.get(TargetOpcode::CFI_INSTRUCTION))
        .addCFIIndex(CFIIndex);
  }
//@ENABLE_GPRESTORE {
#ifdef ENABLE_GPRESTORE
  // Restore GP from the saved stack location
  if (SampleCGFI->needGPSaveRestore()) {
    unsigned Offset = MFI.getObjectOffset(SampleCGFI->getGPFI());
    BuildMI(MBB, MBBI, dl, TII.get(SampleCG::CPRESTORE)).addImm(Offset)
      .addReg(SampleCG::GP);
  }
#endif
//@ENABLE_GPRESTORE }
}
//}

//@emitEpilogue {
void SampleCGSEFrameLowering::emitEpilogue(MachineFunction &MF,
                                 MachineBasicBlock &MBB) const {
  MachineBasicBlock::iterator MBBI = MBB.getLastNonDebugInstr();
  MachineFrameInfo &MFI            = MF.getFrameInfo();
  SampleCGFunctionInfo *SampleCGFI = MF.getInfo<SampleCGFunctionInfo>();

  const SampleCGSEInstrInfo &TII =
      *static_cast<const SampleCGSEInstrInfo *>(STI.getInstrInfo());
  const SampleCGRegisterInfo &RegInfo =
      *static_cast<const SampleCGRegisterInfo *>(STI.getRegisterInfo());

  DebugLoc dl = MBBI->getDebugLoc();
  SampleCGABIInfo ABI = STI.getABI();
  unsigned SP = SampleCG::SP;
  unsigned FP = SampleCG::FP;
  unsigned ZERO = SampleCG::ZERO;
  unsigned ADDu = SampleCG::ADDu;

  // if framepointer enabled, restore the stack pointer.
  if (hasFP(MF)) {
    // Find the first instruction that restores a callee-saved register.
    MachineBasicBlock::iterator I = MBBI;

    for (unsigned i = 0; i < MFI.getCalleeSavedInfo().size(); ++i)
      --I;

    // Insert instruction "move $sp, $fp" at this location.
    BuildMI(MBB, I, dl, TII.get(ADDu), SP).addReg(FP).addReg(ZERO);
  }

  if (SampleCGFI->callsEhReturn()) {
    const TargetRegisterClass *RC = &SampleCG::GPROutRegClass;

    // Find first instruction that restores a callee-saved register.
    MachineBasicBlock::iterator I = MBBI;
    for (unsigned i = 0; i < MFI.getCalleeSavedInfo().size(); ++i)
      --I;

    // Insert instructions that restore eh data registers.
    for (int J = 0; J < ABI.EhDataRegSize(); ++J) {
      TII.loadRegFromStackSlot(MBB, I, ABI.GetEhDataReg(J),
                               SampleCGFI->getEhDataRegFI(J), RC, &RegInfo);
    }
  }

  // Get the number of bytes from FrameInfo
  uint64_t StackSize = MFI.getStackSize();

  if (!StackSize)
    return;

  // Adjust stack.
  TII.adjustStackPtr(SP, StackSize, MBB, MBBI);
}
//}

const SampleCGFrameLowering *
llvm::createSampleCGSEFrameLowering(const SampleCGSubtarget &ST) {
  return new SampleCGSEFrameLowering(ST);
}

/// Mark \p Reg and all registers aliasing it in the bitset.
static void setAliasRegs(MachineFunction &MF, BitVector &SavedRegs,
                         unsigned Reg) {
  const TargetRegisterInfo *TRI = MF.getSubtarget().getRegisterInfo();
  for (MCRegAliasIterator AI(Reg, TRI, true); AI.isValid(); ++AI)
    SavedRegs.set(*AI);
}

void SampleCGSEFrameLowering::determineCalleeSaves(MachineFunction &MF,
													BitVector &SavedRegs,
													RegScavenger *RS) const {
	TargetFrameLowering::determineCalleeSaves(MF, SavedRegs, RS);
	//@determineCalleeSaves-body
	TargetFrameLowering::determineCalleeSaves(MF, SavedRegs, RS);
	SampleCGFunctionInfo *SampleCGFI = MF.getInfo<SampleCGFunctionInfo>();
	//MachineRegisterInfo& MRI = MF.getRegInfo();
	unsigned FP = SampleCG::FP;


	// Mark $fp as used if function has dedicated frame pointer.
  	if (hasFP(MF))
    	setAliasRegs(MF, SavedRegs, FP);

	//@callsEhReturn
	// Create spill slots for eh data registers if function calls eh_return.
	if (SampleCGFI->callsEhReturn())
		SampleCGFI->createEhDataRegsFI();


	if (MF.getFrameInfo().hasCalls()) {
		setAliasRegs(MF, SavedRegs, SampleCG::LR);
	}
	return;
}

bool SampleCGSEFrameLowering::
spillCalleeSavedRegisters(MachineBasicBlock &MBB,
                          MachineBasicBlock::iterator MI,
                          const std::vector<CalleeSavedInfo> &CSI,
                          const TargetRegisterInfo *TRI) const {
  MachineFunction *MF = MBB.getParent();
  MachineBasicBlock *EntryBlock = &MF->front();
  const TargetInstrInfo &TII = *MF->getSubtarget().getInstrInfo();

  for (unsigned i = 0, e = CSI.size(); i != e; ++i) {
    // Add the callee-saved register as live-in. Do not add if the register is
    // LR and return address is taken, because it has already been added in
    // method SampleCGTargetLowering::LowerRETURNADDR.
    // It's killed at the spill, unless the register is LR and return address
    // is taken.
    unsigned Reg = CSI[i].getReg();
    bool IsRAAndRetAddrIsTaken = (Reg == SampleCG::LR)
        && MF->getFrameInfo().isReturnAddressTaken();
    if (!IsRAAndRetAddrIsTaken)
      EntryBlock->addLiveIn(Reg);

    // Insert the spill to the stack frame.
    bool IsKill = !IsRAAndRetAddrIsTaken;
    const TargetRegisterClass *RC = TRI->getMinimalPhysRegClass(Reg);
    TII.storeRegToStackSlot(*EntryBlock, MI, Reg, IsKill,
                            CSI[i].getFrameIdx(), RC, TRI);
  }

  return true;
}

//@hasReservedCallFrame {
bool
SampleCGSEFrameLowering::hasReservedCallFrame(const MachineFunction &MF) const {
  const MachineFrameInfo &MFI = MF.getFrameInfo();

  // Reserve call frame if the size of the maximum call frame fits into 16-bit
  // immediate field and there are no variable sized objects on the stack.
  // Make sure the second register scavenger spill slot can be accessed with one
  // instruction.
  return isInt<16>(MFI.getMaxCallFrameSize() + getStackAlignment()) &&
    !MFI.hasVarSizedObjects();
}
//}

