//===-- SampleCGSEISelDAGToDAG.h - A Dag to Dag Inst Selector for SampleCGSE -----===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// Subclass of SampleCGDAGToDAGISel specialized for samplecg32/64.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_SAMPLECG_SAMPLECGSEISELDAGTODAG_H
#define LLVM_LIB_TARGET_SAMPLECG_SAMPLECGSEISELDAGTODAG_H

#include "SampleCGISelDAGToDAG.h"

namespace llvm {

class SampleCGSEDAGToDAGISel : public SampleCGDAGToDAGISel {

public:
  explicit SampleCGSEDAGToDAGISel(SampleCGTargetMachine &TM, CodeGenOpt::Level OL)
      : SampleCGDAGToDAGISel(TM, OL) {}

  void selectAddESubE(unsigned MOp, SDValue InFlag,
                                        SDValue CmpLHS, const SDLoc &DL, SDNode*Node) const;

private:

  bool runOnMachineFunction(MachineFunction &MF) override;

  bool trySelect(SDNode *Node) override;

  void processFunctionAfterISel(MachineFunction &MF) override;

  // Insert instructions to initialize the global base register in the
  // first MBB of the function.
  void initGlobalBaseReg(MachineFunction &MF);

  bool SelectInlineAsmMemoryOperand(const SDValue &Op,
                                    unsigned ConstraintID,
                                    std::vector<SDValue> &OutOps) override;
  std::pair<SDNode*, SDNode*> selectMULT(SDNode*N, unsigned Opc,
		  						const SDLoc &DL, EVT Ty, bool HasLo,bool HasHi);
};

FunctionPass *createSampleCGSEISelDag(SampleCGTargetMachine &TM,
                                  CodeGenOpt::Level OptLevel);
}

#endif
