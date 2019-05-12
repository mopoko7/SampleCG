//===-- SampleCGFrameLowering.h - Define frame lowering for SampleCG ----*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
//
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_SampleCG_SampleCGFRAMELOWERING_H
#define LLVM_LIB_TARGET_SampleCG_SampleCGFRAMELOWERING_H

#include "SampleCG.h"
#include "llvm/CodeGen/TargetFrameLowering.h"

namespace llvm {
  class SampleCGSubtarget;

class SampleCGFrameLowering : public TargetFrameLowering {
protected:
		  const SampleCGSubtarget &STI;

public:
  explicit SampleCGFrameLowering(const SampleCGSubtarget &sti, unsigned Alignment)
    : TargetFrameLowering(StackGrowsDown, Alignment, 0, Alignment), STI(sti) {}

  static const SampleCGFrameLowering *create(const SampleCGSubtarget &ST);

  bool hasFP(const MachineFunction &MF) const override;

  MachineBasicBlock::iterator
  eliminateCallFramePseudoInstr(MachineFunction &MF,
		  						MachineBasicBlock &MBB,
								MachineBasicBlock::iterator I) const override;
};

/// Create SampleCGFrameLowering objects.
const SampleCGFrameLowering *createSampleCGSEFrameLowering(const SampleCGSubtarget &ST);

} // End llvm namespace

#endif
