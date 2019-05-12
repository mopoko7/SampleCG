//===- SampleCGSEFrameLowering.h - SampleCG32/64 frame lowering ---------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_SAMPLECG_SAMPLECGSEFRAMELOWERING_H
#define LLVM_LIB_TARGET_SAMPLECG_SAMPLECGSEFRAMELOWERING_H

#include "SampleCGFrameLowering.h"
#include <vector>

namespace llvm {

class MachineBasicBlock;
class MachineFunction;
class SampleCGSubtarget;

class SampleCGSEFrameLowering : public SampleCGFrameLowering {
public:
  explicit SampleCGSEFrameLowering(const SampleCGSubtarget &STI);

  /// emitProlog/emitEpilog - These methods insert prolog and epilog code into
  /// the function.
  void emitPrologue(MachineFunction &MF, MachineBasicBlock &MBB) const override;
  void emitEpilogue(MachineFunction &MF, MachineBasicBlock &MBB) const override;
  bool hasReservedCallFrame(const MachineFunction &MF) const;
  void determineCalleeSaves(MachineFunction &MF,BitVector &SavedRegs, RegScavenger *RS) const;
  bool spillCalleeSavedRegisters(MachineBasicBlock &MBB,
		  						 MachineBasicBlock::iterator MI,
								 const std::vector<CalleeSavedInfo> &CSI,
								 const TargetRegisterInfo*TRI) const override;
};

} // end namespace llvm

#endif // LLVM_LIB_TARGET_SAMPLECG_SAMPLECGSEFRAMELOWERING_H
