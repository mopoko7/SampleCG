//===- SampleCGSEISelLowering.cpp - SampleCGSE DAG Lowering Interface -------------===//
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

#include "SampleCGSEISelLowering.h"
#include "SampleCGMachineFunction.h"
#include "SampleCGRegisterInfo.h"
#include "SampleCGSubtarget.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Triple.h"
#include "llvm/CodeGen/CallingConvLower.h"
#include "llvm/CodeGen/ISDOpcodes.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineMemOperand.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/SelectionDAG.h"
#include "llvm/CodeGen/SelectionDAGNodes.h"
#include "llvm/CodeGen/TargetInstrInfo.h"
#include "llvm/CodeGen/TargetSubtargetInfo.h"
#include "llvm/CodeGen/ValueTypes.h"
#include "llvm/IR/DebugLoc.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/MachineValueType.h"
#include "llvm/Support/MathExtras.h"
#include "llvm/Support/raw_ostream.h"
#include <algorithm>
#include <cassert>
#include <cstdint>
#include <iterator>
#include <utility>

using namespace llvm;

#define DEBUG_TYPE "samplecg-isel"

static cl::opt<bool>
EnableSampleCGTailCalls("enable-samplecg-tail-calls", cl::Hidden,
                    cl::desc("samplecg: Enable tail calls."), cl::init(false));


SampleCGSETargetLowering::SampleCGSETargetLowering(const SampleCGTargetMachine &TM,
                                           const SampleCGSubtarget &STI)
    : SampleCGTargetLowering(TM, STI) {
  // Set up the register classes
  addRegisterClass(MVT::i32, &SampleCG::CPURegsRegClass); 

  setOperationAction(ISD::ATOMIC_FENCE,       MVT::Other, Custom);

  computeRegisterProperties(Subtarget.getRegisterInfo());
}

SDValue SampleCGSETargetLowering::LowerOperation(SDValue Op,
                                             SelectionDAG &DAG) const {
  return SampleCGTargetLowering::LowerOperation(Op, DAG);
}

const SampleCGTargetLowering *
llvm::createSampleCGSETargetLowering(const SampleCGTargetMachine &TM,
                                 const SampleCGSubtarget &STI) {
  return new SampleCGSETargetLowering(TM, STI);
}

bool SampleCGSETargetLowering::
isEligibleForTailCallOptimization(const SampleCGCC &SampleCGCCInfo,
                                  unsigned NextStackOffset,
                                  const SampleCGFunctionInfo& FI) const {
  if (!EnableSampleCGTailCalls)
    return false;

  // Return false if either the callee or caller has a byval argument.
  if (SampleCGCCInfo.hasByValArg() || FI.hasByvalArg())
    return false;

  // Return true if the callee's argument area is no larger than the
  // caller's.
  return NextStackOffset <= FI.getIncomingArgSize();
}

