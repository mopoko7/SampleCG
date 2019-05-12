//===- SampleCGSEISelLowering.h - SampleCGSE DAG Lowering Interface -----*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// Subclass of SampleCGTargetLowering specialized for samplecg32/64.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_SAMPLECG_SAMPLECGSEISELLOWERING_H
#define LLVM_LIB_TARGET_SAMPLECG_SAMPLECGSEISELLOWERING_H

#include "SampleCGISelLowering.h"
#include "llvm/CodeGen/SelectionDAGNodes.h"
#include "llvm/Support/MachineValueType.h"

namespace llvm {

class MachineBasicBlock;
class MachineInstr;
class SampleCGSubtarget;
class SampleCGTargetMachine;
class SelectionDAG;
class TargetRegisterClass;

  class SampleCGSETargetLowering : public SampleCGTargetLowering  {
  public:
    explicit SampleCGSETargetLowering(const SampleCGTargetMachine &TM,
                                  const SampleCGSubtarget &STI);

    SDValue LowerOperation(SDValue Op, SelectionDAG &DAG) const override;
  private:
	bool isEligibleForTailCallOptimization(const SampleCGCC &SampleCGCCInfo,
			unsigned NextStackOffset,
			const SampleCGFunctionInfo& FI) const override;


  };

} // end namespace llvm

#endif // LLVM_LIB_TARGET_SAMPLECG_SAMPLECGSEISELLOWERING_H
