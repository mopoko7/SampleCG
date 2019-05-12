//===-- SampleCG.h - Top-level interface for SampleCG representation --*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the entry points for global functions defined in the LLVM
// SampleCG back-end.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_SAMPLECG_SAMPLECG_H
#define LLVM_LIB_TARGET_SAMPLECG_SAMPLECG_H

#include "MCTargetDesc/SampleCGMCTargetDesc.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Target/TargetMachine.h"

namespace llvm {
  class FunctionPass;
  class SampleCGTargetMachine;

	FunctionPass *createSampleCGLongBranchPass(SampleCGTargetMachine &TM);
	FunctionPass *createSampleCGDelJmpPass(SampleCGTargetMachine &TM);
	FunctionPass *createSampleCGDelaySlotFillerPass(SampleCGTargetMachine &TM);
#ifdef ENABLE_GPRESTORE
	FunctionPass *createSampleCGEmitGPRestorePass(SampleCGTargetMachine &TM);
#endif
} // end namespace llvm;


#endif
