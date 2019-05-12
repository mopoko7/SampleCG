//===---- SampleCGABIInfo.cpp - Information about SAMPLECG ABI's ------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "SampleCGABIInfo.h"
#include "SampleCGRegisterInfo.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/MC/MCTargetOptions.h"

using namespace llvm;

// Note: this option is defined here to be visible from libLLVMSampleCGAsmParser
//       and libLLVMSampleCGCodeGen
static cl::opt<bool>
EnableSampleCGS32Calls("samplecg-s32-calls", cl::Hidden,
              cl::desc("SAMPLECG S32 call: use stack only to pass arguments"),
              cl::init(false));

namespace {
static const MCPhysReg O32IntRegs[4] = {SampleCG::A0, SampleCG::A1};

static const MCPhysReg S32IntRegs = {};

}

ArrayRef<MCPhysReg> SampleCGABIInfo::GetByValArgRegs() const {
  if (IsO32())
    return makeArrayRef(O32IntRegs);
  if (IsO32())
    return makeArrayRef(S32IntRegs);
  llvm_unreachable("Unhandled ABI");
}

ArrayRef<MCPhysReg> SampleCGABIInfo::GetVarArgRegs() const {
  if (IsO32())
    return makeArrayRef(O32IntRegs);
  if (IsS32())
    return makeArrayRef(S32IntRegs);
  llvm_unreachable("Unhandled ABI");
}

unsigned SampleCGABIInfo::GetCalleeAllocdArgSizeInBytes(CallingConv::ID CC) const {
  if (IsO32())
    return CC != 0;
  if (IsS32())
    return 0;
  llvm_unreachable("Unhandled ABI");
}

SampleCGABIInfo SampleCGABIInfo::computeTargetABI(const Triple &TT, StringRef CPU,
                                          const MCTargetOptions &Options) {
	SampleCGABIInfo abi(ABI::Unknown);

	if (EnableSampleCGS32Calls) {
		abi = ABI::S32;
	}
	else {
		abi = ABI::O32;
	}
	return abi;
}

unsigned SampleCGABIInfo::GetStackPtr() const {
  return SampleCG::SP;
}

unsigned SampleCGABIInfo::GetFramePtr() const {
  return SampleCG::FP;
}

unsigned SampleCGABIInfo::GetNullPtr() const {
  return SampleCG::ZERO;
}

unsigned SampleCGABIInfo::GetEhDataReg(unsigned I) const {
  static const unsigned EhDataReg[] = {
    SampleCG::A0, SampleCG::A1
  };
  return EhDataReg[I];
}

int SampleCGABIInfo::EhDataRegSize() const {
	if (ThisABI == ABI::S32) {
		return 0;
	}
	else {
		return 2;
	}
}

