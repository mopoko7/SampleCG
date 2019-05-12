//===- SampleCGRegisterInfo.cpp - SAMPLECG Register Information -------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the SAMPLECG implementation of the TargetRegisterInfo class.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "samplecg-reg-info"

#include "SampleCGRegisterInfo.h"
#include "MCTargetDesc/SampleCGABIInfo.h"
#include "SampleCG.h"
#include "SampleCGMachineFunction.h"
#include "SampleCGSubtarget.h"
#include "SampleCGTargetMachine.h"
#include "llvm/ADT/BitVector.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/TargetFrameLowering.h"
#include "llvm/CodeGen/TargetRegisterInfo.h"
#include "llvm/CodeGen/TargetSubtargetInfo.h"
#include "llvm/IR/Function.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
#include <cstdint>

using namespace llvm;

#define GET_REGINFO_TARGET_DESC
#include "SampleCGGenRegisterInfo.inc"

SampleCGRegisterInfo::SampleCGRegisterInfo(const SampleCGSubtarget &ST)
	: SampleCGGenRegisterInfo(SampleCG::LR), Subtarget(ST) {}

const TargetRegisterClass *
SampleCGRegisterInfo::getPointerRegClass(const MachineFunction &MF,
                                     unsigned Kind) const {
  return &SampleCG::CPURegsRegClass;
}

//===----------------------------------------------------------------------===//
// Callee Saved Registers methods
//===----------------------------------------------------------------------===//

/// SampleCG Callee Saved Registers
const MCPhysReg *
SampleCGRegisterInfo::getCalleeSavedRegs(const MachineFunction *MF) const {
  return CSR_O32_SaveList;
}

const uint32_t *
SampleCGRegisterInfo::getCallPreservedMask(const MachineFunction &MF,
                                       CallingConv::ID) const {
  return CSR_O32_RegMask;
}

BitVector SampleCGRegisterInfo::
getReservedRegs(const MachineFunction &MF) const {
  static const MCPhysReg ReservedCPURegs[] = {
    SampleCG::ZERO, SampleCG::AT, SampleCG::SP, SampleCG::LR, SampleCG::PC
  };
  BitVector Reserved(getNumRegs());

  for (unsigned I = 0; I < array_lengthof(ReservedCPURegs); ++I)
    Reserved.set(ReservedCPURegs[I]);

  // Reserve FP if this function should have a dedicated frame pointer register.
  if (MF.getSubtarget().getFrameLowering()->hasFP(MF)) {
    Reserved.set(SampleCG::FP);
  }

#ifdef ENABLE_GPRESTORE //1
  const SampleCGFunctionInfo *SampleCGFI = MF.getInfo<SampleCGFunctionInfo>();
  // Reserve GP if globalBaseRegFixed()
  if (SampleCGFI->globalBaseRegFixed())
#endif
  	Reserved.set(SampleCG::GP);

  return Reserved;
}

//@eliminateFrameIndex {
//- If no eliminateFrameIndex(), it will hang on run. 
// pure virtual method
// FrameIndex represent objects inside a abstract stack.
// We must replace FrameIndex with an stack/frame pointer
// direct reference.
void SampleCGRegisterInfo::
eliminateFrameIndex(MachineBasicBlock::iterator II, int SPAdj,
                    unsigned FIOperandNum, RegScavenger *RS) const {
  MachineInstr &MI = *II;
  MachineFunction &MF = *MI.getParent()->getParent();
  MachineFrameInfo &MFI = MF.getFrameInfo();
  SampleCGFunctionInfo *SampleCGFI = MF.getInfo<SampleCGFunctionInfo>();

  unsigned i = 0;
  while (!MI.getOperand(i).isFI()) {
    ++i;
    assert(i < MI.getNumOperands() &&
           "Instr doesn't have FrameIndex operand!");
  }

  LLVM_DEBUG(errs() << "\nFunction : " << MF.getFunction().getName() << "\n";
        errs() << "<--------->\n" << MI);

  int FrameIndex = MI.getOperand(i).getIndex();
  uint64_t stackSize = MF.getFrameInfo().getStackSize();
  int64_t spOffset = MF.getFrameInfo().getObjectOffset(FrameIndex);

  LLVM_DEBUG(errs() << "FrameIndex : " << FrameIndex << "\n"
               << "spOffset   : " << spOffset << "\n"
               << "stackSize  : " << stackSize << "\n");

  const std::vector<CalleeSavedInfo> &CSI = MFI.getCalleeSavedInfo();
  int MinCSFI = 0;
  int MaxCSFI = -1;

  if (CSI.size()) {
    MinCSFI = CSI[0].getFrameIdx();
    MaxCSFI = CSI[CSI.size() - 1].getFrameIdx();
  }

  // The following stack frame objects are always referenced relative to $sp:
  //  1. Outgoing arguments.
  //  2. Pointer to dynamically allocated stack space.
  //  3. Locations for callee-saved registers.
  // Everything else is referenced relative to whatever register
  // getFrameRegister() returns.
  unsigned FrameReg;

  if (SampleCGFI->isOutArgFI(FrameIndex) || SampleCGFI->isDynAllocFI(FrameIndex) ||
      (FrameIndex >= MinCSFI && FrameIndex <= MaxCSFI))
    FrameReg = SampleCG::SP;
  else
    FrameReg = getFrameRegister(MF);

  // Calculate final offset.
  // - There is no need to change the offset if the frame object is one of the
  //   following: an outgoing argument, pointer to a dynamically allocated
  //   stack space or a $gp restore location,
  // - If the frame object is any of the following, its offset must be adjusted
  //   by adding the size of the stack:
  //   incoming argument, callee-saved register location or local variable.
  int64_t Offset;
#ifdef ENABLE_GPRESTORE //2
  if (SampleCGFI->isOutArgFI(FrameIndex) || SampleCGFI->isGPFI(FrameIndex) ||
      SampleCGFI->isDynAllocFI(FrameIndex))
    Offset = spOffset;
  else
#endif
    Offset = spOffset + (int64_t)stackSize;

  Offset    += MI.getOperand(i+1).getImm();

  LLVM_DEBUG(errs() << "Offset     : " << Offset << "\n" << "<--------->\n");

  // If MI is not a debug value, make sure Offset fits in the 16-bit immediate
  // field.
  if (!MI.isDebugValue() && !isInt<16>(Offset)) {
	assert("(!MI.isDebugValue() && !isInt<16>(Offset))");
  }

  MI.getOperand(i).ChangeToRegister(FrameReg, false);
  MI.getOperand(i+1).ChangeToImmediate(Offset);
}
//}
bool
SampleCGRegisterInfo::requiresRegisterScavenging(const MachineFunction &MF) const {
	return true;
}

bool
SampleCGRegisterInfo::trackLivenessAfterRegAlloc(const MachineFunction &MF) const {
	return true;
}

unsigned SampleCGRegisterInfo::
getFrameRegister(const MachineFunction &MF) const {
  const TargetFrameLowering *TFI = Subtarget.getFrameLowering();
  return TFI->hasFP(MF) ? SampleCG::FP : SampleCG::SP;
}
