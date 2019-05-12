//===- SampleCGRegisterInfo.h - SampleCG Register Information Impl ------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the SampleCG implementation of the TargetRegisterInfo class.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_SAMPLECG_SAMPLECGREGISTERINFO_H
#define LLVM_LIB_TARGET_SAMPLECG_SAMPLECGREGISTERINFO_H

#include "SampleCG.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include <cstdint>

#define GET_REGINFO_HEADER
#include "SampleCGGenRegisterInfo.inc"

namespace llvm {

class SampleCGSubtarget;

class TargetRegisterClass;

class SampleCGRegisterInfo : public SampleCGGenRegisterInfo {
protected:
	const SampleCGSubtarget &Subtarget;

public:
  SampleCGRegisterInfo(const SampleCGSubtarget &Subtarget);

  /// Code Generation virtual methods...
  const TargetRegisterClass *getPointerRegClass(const MachineFunction &MF,
                                                unsigned Kind) const override;

  const MCPhysReg *getCalleeSavedRegs(const MachineFunction *MF) const override;

  const uint32_t *getCallPreservedMask(const MachineFunction &MF,
                                       CallingConv::ID) const override;

  BitVector getReservedRegs(const MachineFunction &MF) const override;

  bool requiresRegisterScavenging(const MachineFunction &MF) const override;

  bool trackLivenessAfterRegAlloc(const MachineFunction &MF) const override;

  /// Stack Frame Processing Methods
  void eliminateFrameIndex(MachineBasicBlock::iterator II,
                           int SPAdj, unsigned FIOperandNum,
                           RegScavenger *RS = nullptr) const override;

  /// Debug information queries.
  unsigned getFrameRegister(const MachineFunction &MF) const override;

  /// Return GPR register class.
  virtual const TargetRegisterClass *intRegClass(unsigned Size) const = 0;
};

} // end namespace llvm

#endif // LLVM_LIB_TARGET_SAMPLECG_SAMPLECGREGISTERINFO_H
