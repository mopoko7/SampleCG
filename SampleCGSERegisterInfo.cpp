//===-- SampleCGSERegisterInfo.cpp - SAMPLECG32/64 Register Information -== -------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the SAMPLECG32/64 implementation of the TargetRegisterInfo
// class.
//
//===----------------------------------------------------------------------===//

#include "SampleCGSERegisterInfo.h"
#include "SampleCG.h"
#include "SampleCGMachineFunction.h"
#include "SampleCGSubtarget.h"
#include "SampleCGTargetMachine.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/TargetFrameLowering.h"
#include "llvm/CodeGen/TargetInstrInfo.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DebugInfo.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Type.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"

using namespace llvm;

#define DEBUG_TYPE "samplecg-reg-info"

SampleCGSERegisterInfo::SampleCGSERegisterInfo(const SampleCGSubtarget &ST) : SampleCGRegisterInfo(ST) {}

const TargetRegisterClass *SampleCGSERegisterInfo::intRegClass(unsigned Size) const {
	return &SampleCG::CPURegsRegClass;
}
