//===-- SampleCGSEInstrInfo.cpp - SampleCG32/64 Instruction Information -----------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the SampleCG32/64 implementation of the TargetInstrInfo class.
//
//===----------------------------------------------------------------------===//

#include "SampleCGSubtarget.h"
#include "SampleCGSEInstrInfo.h"
#include "SampleCGMachineFunction.h"
#include "SampleCGTargetMachine.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/MathExtras.h"
#include "llvm/Support/TargetRegistry.h"

using namespace llvm;

SampleCGSEInstrInfo::SampleCGSEInstrInfo(const SampleCGSubtarget &STI) :  SampleCGInstrInfo(STI), RI(STI) {}

const SampleCGRegisterInfo &SampleCGSEInstrInfo::getRegisterInfo() const {
  return RI;
}

void SampleCGSEInstrInfo::expandEhReturn(MachineBasicBlock &MBB,
                                     MachineBasicBlock::iterator I) const {
  // This pseudo instruction is generated as part of the lowering of
  // ISD::EH_RETURN. We convert it to a stack increment by OffsetReg, and
  // indirect jump to TargetReg
  unsigned ADDU = SampleCG::ADDu;
  unsigned SP = SampleCG::SP;
  unsigned LR = SampleCG::LR;
  unsigned T9 = SampleCG::T9;
  unsigned ZERO = SampleCG::ZERO;
  unsigned OffsetReg = I->getOperand(0).getReg();
  unsigned TargetReg = I->getOperand(1).getReg();

  // addu $lr, $v0, $zero
  // addu $sp, $sp, $v1
  // jr   $lr (via RetLR)
  const TargetMachine &TM = MBB.getParent()->getTarget();
  if (TM.isPositionIndependent())
    BuildMI(MBB, I, I->getDebugLoc(), get(ADDU), T9)
        .addReg(TargetReg)
        .addReg(ZERO);
  BuildMI(MBB, I, I->getDebugLoc(), get(ADDU), LR)
      .addReg(TargetReg)
      .addReg(ZERO);
  BuildMI(MBB, I, I->getDebugLoc(), get(ADDU), SP).addReg(SP).addReg(OffsetReg);
  expandRetLR(MBB, I);
}

bool SampleCGSEInstrInfo::expandPostRAPseudo(MachineInstr &MI) const {
  MachineBasicBlock &MBB = *MI.getParent();

  switch (MI.getDesc().getOpcode()) {
  default:
    return false;
  case SampleCG::RetLR:
	expandRetLR(MBB, MI);
    break;
  case SampleCG::SAMPLECGeh_return32:
    expandEhReturn(MBB, MI);
    break;
  }

  MBB.erase(MI);
  return true;
}

void SampleCGSEInstrInfo::expandRetLR(MachineBasicBlock &MBB, MachineBasicBlock::iterator I) const {
	BuildMI(MBB, I, I->getDebugLoc(), get(SampleCG::RET)).addReg(SampleCG::LR);
}

const SampleCGInstrInfo *llvm::createSampleCGSEInstrInfo(const SampleCGSubtarget &STI) {
  return new SampleCGSEInstrInfo(STI);
}

void SampleCGSEInstrInfo::adjustStackPtr(unsigned SP, int64_t Amount,
											MachineBasicBlock &MBB,
											MachineBasicBlock::iterator I) const {
	DebugLoc DL = (I != MBB.end()) ? I->getDebugLoc() : DebugLoc();
	unsigned ADDu = SampleCG::ADDu;
	unsigned ADDiu = SampleCG::ADDiu;

	if (isInt<16>(Amount)) {
		BuildMI(MBB, I, DL, get(ADDiu), SP).addReg(SP).addImm(Amount);
	}
	else {
		unsigned Reg = loadImmediate(Amount, MBB, I, DL, nullptr);
		BuildMI(MBB, I, DL, get(ADDu), SP).addReg(SP).addReg(Reg, RegState::Kill);
	}
}

unsigned SampleCGSEInstrInfo::loadImmediate(int64_t Imm, MachineBasicBlock &MBB,
											MachineBasicBlock::iterator II,
											const DebugLoc &DL,
											unsigned *NewImm) const {
	SampleCGAnalyzeImmediate AnalyzeImm;
	unsigned Size = 32;
	unsigned LUi = SampleCG::LUi;
	unsigned ZEROReg = SampleCG::ZERO;
	unsigned ATReg = SampleCG::AT;
	bool LastInstrIsADDiu = NewImm;

	const SampleCGAnalyzeImmediate::InstSeq &Seq = 
		AnalyzeImm.Analyze(Imm, Size, LastInstrIsADDiu);
	SampleCGAnalyzeImmediate::InstSeq::const_iterator Inst = Seq.begin();

	assert(Seq.size() && (!LastInstrIsADDiu || (Seq.size() > 1)));

	if (Inst->Opc == LUi) {
		BuildMI(MBB, II, DL, get(LUi), ATReg).addImm(SignExtend64<16>(Inst->ImmOpnd));
	}
	else {
		BuildMI(MBB, II, DL, get(Inst->Opc), ATReg).addReg(ZEROReg).addImm(SignExtend64<16>(Inst->ImmOpnd));
	}

	for (++Inst; Inst != Seq.end() - LastInstrIsADDiu; ++Inst) {
		BuildMI(MBB, II, DL, get(Inst->Opc), ATReg).addReg(ATReg).addImm(SignExtend64<16>(Inst->ImmOpnd));
	}
	if (LastInstrIsADDiu) {
		*NewImm = Inst->ImmOpnd;
	}
	return ATReg;
}

void SampleCGSEInstrInfo::storeRegToStack(MachineBasicBlock &MBB,
											MachineBasicBlock::iterator I,
											unsigned SrcReg, bool isKill, int FI,
											const TargetRegisterClass*RC,
											const TargetRegisterInfo*TRI,
											int64_t Offset) const {
	DebugLoc DL;
	MachineMemOperand*MMO = GetMemOperand(MBB, FI, MachineMemOperand::MOStore);
	unsigned Opc = 0;
	Opc = SampleCG::ST;assert(Opc && "Register class not handled!");
	BuildMI(MBB, I, DL, get(Opc)).addReg(SrcReg, getKillRegState(isKill))
		.addFrameIndex(FI).addImm(Offset).addMemOperand(MMO);
}

void SampleCGSEInstrInfo::loadRegFromStack(MachineBasicBlock &MBB,
										MachineBasicBlock::iterator I,
										unsigned DestReg, int FI,
										const TargetRegisterClass*RC,
										const TargetRegisterInfo*TRI,
										int64_t Offset) const {
	DebugLoc DL;
	if(I != MBB.end()) {
		DL = I->getDebugLoc();
	}
	MachineMemOperand*MMO = GetMemOperand(MBB, FI, MachineMemOperand::MOLoad);
	unsigned Opc = 0;
	Opc = SampleCG::LD;assert(Opc && "Register class not handled!");
	BuildMI(MBB, I, DL, get(Opc), DestReg).addFrameIndex(FI).addImm(Offset)
		.addMemOperand(MMO);
}

void SampleCGSEInstrInfo::copyPhysReg(MachineBasicBlock &MBB,
										MachineBasicBlock::iterator I,
										const DebugLoc &DL,
										unsigned DestReg,
										unsigned SrcReg, bool KillSrc) const {
	unsigned Opc = 0, ZeroReg = 0;
	if(SampleCG::CPURegsRegClass.contains(DestReg)) { // Copy to CPU Reg.
		if(SampleCG::CPURegsRegClass.contains(SrcReg))
			Opc = SampleCG::ADDu, ZeroReg = SampleCG::ZERO;
		else if(SrcReg == SampleCG::HI)
			Opc = SampleCG::MFHI, SrcReg = 0;
		else if(SrcReg == SampleCG::LO)
			Opc = SampleCG::MFLO, SrcReg = 0;
	}
	else if(SampleCG::CPURegsRegClass.contains(SrcReg)) { // Copyfrom CPUReg.
		if(DestReg == SampleCG::HI)
			Opc = SampleCG::MTHI, DestReg = 0;
		else if(DestReg == SampleCG::LO)
			Opc = SampleCG::MTLO, DestReg = 0;
	}

	assert(Opc && "Cannot copy registers");
	
	MachineInstrBuilder MIB = BuildMI(MBB, I, DL, get(Opc));

	if(DestReg)
		MIB.addReg(DestReg, RegState::Define);

	if(ZeroReg)
		MIB.addReg(ZeroReg);

	if(SrcReg)
		MIB.addReg(SrcReg, getKillRegState(KillSrc));
}

/// getOppositeBranchOpc - Return the inverse of the specified
/// opcode, e.g. turning BEQ to BNE.
unsigned SampleCGSEInstrInfo::getOppositeBranchOpc(unsigned Opc) const {
  switch (Opc) {
  default:           llvm_unreachable("Illegal opcode!");
  case SampleCG::BEQ:    return SampleCG::BNE;
  case SampleCG::BNE:    return SampleCG::BEQ;
  }
}
