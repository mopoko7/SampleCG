//===- SampleCGInstrInfo.h - SampleCG Instruction Information -----------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the SampleCG implementation of the TargetInstrInfo class.
//
// FIXME: We need to override TargetInstrInfo::getInlineAsmLength method in
// order for SampleCGLongBranch pass to work correctly when the code has inline
// assembly.  The returned value doesn't have to be the asm instruction's exact
// size in bytes; SampleCGLongBranch only expects it to be the correct upper bound.
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_SAMPLECG_SAMPLECGINSTRINFO_H
#define LLVM_LIB_TARGET_SAMPLECG_SAMPLECGINSTRINFO_H

#include "MCTargetDesc/SampleCGMCTargetDesc.h"
#include "SampleCG.h"
#include "SampleCGAnalyzeImmediate.h"
#include "SampleCGRegisterInfo.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineMemOperand.h"
#include "llvm/CodeGen/TargetInstrInfo.h"
#include <cstdint>

#define GET_INSTRINFO_HEADER
#include "SampleCGGenInstrInfo.inc"

namespace llvm {

class MachineInstr;
class MachineOperand;
class SampleCGSubtarget;
class TargetRegisterClass;
class TargetRegisterInfo;

class SampleCGInstrInfo : public SampleCGGenInstrInfo {
  virtual void anchor();

protected:
  const SampleCGSubtarget &Subtarget;

public:
  explicit SampleCGInstrInfo(const SampleCGSubtarget &STI);

  static  const SampleCGInstrInfo *create(SampleCGSubtarget &STI);

  /// getRegisterInfo - TargetInstrInfo is a superset of MRegister info.  As
  /// such, whenever a client has an instance of instruction info, it should
  /// always be able to get register info as well (through this method).
  virtual const SampleCGRegisterInfo &getRegisterInfo() const = 0;

  virtual unsigned getOppositeBranchOpc(unsigned Opc) const = 0;

  /// Return the number of bytes of code the specified instruction may be.
  unsigned getInstSizeInBytes(const MachineInstr &MI) const override;

  virtual void adjustStackPtr(unsigned SP, int64_t Amount,
                              MachineBasicBlock &MBB,
                              MachineBasicBlock::iterator I) const = 0;
	void storeRegToStackSlot(MachineBasicBlock &MBB,
								MachineBasicBlock::iterator MBBI,
								unsigned SrcReg,
								bool isKill,
								int FrameIndex,
								const TargetRegisterClass*RC,
								const TargetRegisterInfo*TRI) const override {
		storeRegToStack(MBB, MBBI, SrcReg, isKill, FrameIndex, RC, TRI, 0);
	}

	void loadRegFromStackSlot(MachineBasicBlock &MBB,
								MachineBasicBlock::iterator MBBI,
								unsigned DestReg,
								int FrameIndex,
								const TargetRegisterClass*RC,
								const TargetRegisterInfo*TRI) const override {
		loadRegFromStack(MBB, MBBI, DestReg, FrameIndex, RC, TRI, 0);
	}

	virtual void storeRegToStack(MachineBasicBlock &MBB,
									MachineBasicBlock::iterator MI,
									unsigned SrcReg,
									bool isKill,
									int FrameIndex,
									const TargetRegisterClass*RC,
									const TargetRegisterInfo*TRI,
									int64_t Offset) const = 0;
	virtual void loadRegFromStack(MachineBasicBlock &MBB,
									MachineBasicBlock::iterator MI,
									unsigned DestReg,
									int FrameIndex,
									const TargetRegisterClass*RC,
									const TargetRegisterInfo*TRI,
									int64_t Offset) const = 0;
	MachineMemOperand *GetMemOperand(MachineBasicBlock &MBB, int FI,MachineMemOperand::Flags Flags) const;

};

/// Create SampleCGInstrInfo objects.
const SampleCGInstrInfo *createSampleCGSEInstrInfo(const SampleCGSubtarget &STI);

} // end namespace llvm

#endif // LLVM_LIB_TARGET_SAMPLECG_SAMPLECGINSTRINFO_H
