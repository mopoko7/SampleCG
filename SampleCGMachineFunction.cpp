//===-- SampleCGMachineFunctionInfo.cpp - Private data used for SampleCG ----------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "SampleCGMachineFunction.h"
#include "MCTargetDesc/SampleCGABIInfo.h"
#include "SampleCGSubtarget.h"
#include "SampleCGTargetMachine.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/PseudoSourceValue.h"
#include "llvm/CodeGen/TargetRegisterInfo.h"
#include "llvm/Support/CommandLine.h"

using namespace llvm;

static cl::opt<bool>
FixGlobalBaseReg("samplecg-fix-global-base-reg", cl::Hidden, cl::init(true),
                 cl::desc("Always use $gp as the global base register."));

SampleCGFunctionInfo::~SampleCGFunctionInfo() = default;

void SampleCGFunctionInfo::anchor() {}

unsigned SampleCGFunctionInfo::getGlobalBaseReg() {
  return GlobalBaseReg = SampleCG::GP;
}

void SampleCGFunctionInfo::createEhDataRegsFI() {
  const TargetRegisterInfo &TRI = *MF.getSubtarget().getRegisterInfo();
  for (int I = 0; I < 2; ++I) {
    const TargetRegisterClass *RC = &SampleCG::CPURegsRegClass;

    EhDataRegFI[I] = MF.getFrameInfo().CreateStackObject(TRI.getSpillSize(*RC),
        TRI.getSpillAlignment(*RC), false);
  }
}

bool SampleCGFunctionInfo::globalBaseRegFixed() const {
	return FixGlobalBaseReg;
}

bool SampleCGFunctionInfo::globalBaseRegSet() const {
	return GlobalBaseReg;
}

MachinePointerInfo SampleCGFunctionInfo::callPtrInfo(const char *ES) {
  return MachinePointerInfo(MF.getPSVManager().getExternalSymbolCallEntry(ES));
}

MachinePointerInfo SampleCGFunctionInfo::callPtrInfo(const GlobalValue *GV) {
  return MachinePointerInfo(MF.getPSVManager().getGlobalValueCallEntry(GV));
}

