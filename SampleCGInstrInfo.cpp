//===- SampleCGInstrInfo.cpp - SampleCG Instruction Information -------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the SampleCG implementation of the TargetInstrInfo class.
//
//===----------------------------------------------------------------------===//

#include "SampleCGInstrInfo.h"
#include "MCTargetDesc/SampleCGMCTargetDesc.h"
#include "SampleCGSubtarget.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineOperand.h"
#include "llvm/CodeGen/TargetOpcodes.h"
#include "llvm/CodeGen/TargetSubtargetInfo.h"
#include "llvm/IR/DebugLoc.h"
#include "llvm/MC/MCInstrDesc.h"
#include "llvm/Target/TargetMachine.h"
#include <cassert>

using namespace llvm;

#define GET_INSTRINFO_CTOR_DTOR
#include "SampleCGGenInstrInfo.inc"

// Pin the vtable to this file.
void SampleCGInstrInfo::anchor() {}

SampleCGInstrInfo::SampleCGInstrInfo(const SampleCGSubtarget &STI) : 
	SampleCGGenInstrInfo(SampleCG::ADJCALLSTACKDOWN, SampleCG::ADJCALLSTACKUP),
	Subtarget(STI) {}

const SampleCGInstrInfo *SampleCGInstrInfo::create(SampleCGSubtarget &STI) {
  return createSampleCGSEInstrInfo(STI);
}

/// Return the number of bytes of code the specified instruction may be.
unsigned SampleCGInstrInfo::getInstSizeInBytes(const MachineInstr &MI) const {
  switch (MI.getOpcode()) {
  default:
    return MI.getDesc().getSize();
  case  TargetOpcode::INLINEASM: {       // Inline Asm: Variable size.
    const MachineFunction *MF = MI.getParent()->getParent();
    const char *AsmStr = MI.getOperand(0).getSymbolName();
    return getInlineAsmLength(AsmStr, *MF->getTarget().getMCAsmInfo());
  }
  }
}

MachineMemOperand*
SampleCGInstrInfo::GetMemOperand(MachineBasicBlock &MBB, int FI,
									MachineMemOperand::Flags Flags) const {
	MachineFunction &MF =*MBB.getParent();
	MachineFrameInfo &MFI = MF.getFrameInfo();
	unsigned Align = MFI.getObjectAlignment(FI);
	
	return MF.getMachineMemOperand(MachinePointerInfo::getFixedStack(MF, FI),
			Flags, MFI.getObjectSize(FI), Align);
}
