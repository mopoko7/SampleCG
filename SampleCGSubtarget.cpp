//===-- SampleCGSubtarget.cpp - SampleCG Subtarget Information --------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file implements the SampleCG specific subclass of TargetSubtargetInfo.
//
//===----------------------------------------------------------------------===//

#include "SampleCGSubtarget.h"
#include "SampleCG.h"
#include "SampleCGMachineFunction.h"
#include "SampleCGRegisterInfo.h"
#include "SampleCGTargetMachine.h"
#include "llvm/IR/Attributes.h"
#include "llvm/IR/Function.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

#define DEBUG_TYPE "samplecg-subtarget"

#define GET_SUBTARGETINFO_TARGET_DESC
#define GET_SUBTARGETINFO_CTOR
#include "SampleCGGenSubtargetInfo.inc"

static cl::opt<bool> EnableOverflowOpt("samplecg-enable-overflow", cl::Hidden, cl::init(false),
		cl::desc("Use trigger overflow instructions add and sub instead of non-overflow instructions addu and subu"));

static cl::opt<bool> UseSmallSectionOpt
				("samplecg-use-small-section", cl::Hidden, cl::init(false),
				 cl::desc("Use small section. Only work when -relocation-model="
					 "static. pic always not use small section."));
				
static cl::opt<bool> ReserveGPOpt("samplecg-reserve-gp", cl::Hidden, cl::init(false),
				cl::desc("Never allocate $gp to variable"));

static cl::opt<bool> NoCploadOpt("samplecg-no-cpload", cl::Hidden, cl::init(false),
				cl::desc("No issue .cpload"));

bool SampleCGReserveGP;

bool SampleCGNoCpload;

extern bool FixGlobalBaseReg;

void SampleCGSubtarget::anchor() {}

SampleCGSubtarget::SampleCGSubtarget(const Triple &TT, StringRef CPU, StringRef FS,
                             bool little, const SampleCGTargetMachine &TM,
                             unsigned StackAlignOverride)
    : SampleCGGenSubtargetInfo(TT, CPU, FS),
      IsLittle(little),
      TM(TM), TargetTriple(TT), TSInfo(),
      InstrInfo(
          SampleCGInstrInfo::create(initializeSubtargetDependencies(CPU, FS, TM))),
      FrameLowering(SampleCGFrameLowering::create(*this)),
      TLInfo(SampleCGTargetLowering::create(TM, *this)) {
	EnableOverflow = EnableOverflowOpt;
	UseSmallSection = UseSmallSectionOpt;
	SampleCGReserveGP = ReserveGPOpt;
	SampleCGNoCpload = NoCploadOpt;
}

bool SampleCGSubtarget::isPositionIndependent() const {
  return TM.isPositionIndependent();
}

SampleCGSubtarget &
SampleCGSubtarget::initializeSubtargetDependencies(StringRef CPU, StringRef FS,
                                               const TargetMachine &TM) {
	if (TargetTriple.getArch() == Triple::samplecg || TargetTriple.getArch() == Triple::samplecgel) {
		if (CPU.empty() || CPU == "generic") {
			CPU = "samplecg32II";
		}
		else if (CPU == "help") {
			CPU = "";
			return *this;
		}
		else if (CPU != "samplecg32I"  && CPU != "samplecg32II") {
			CPU = "samplecg32II";
		}
	}
	else {
		errs() << "!!!Error, TargetTriple.getArdch() == " << TargetTriple.getArch()
				<< "CPU = " << CPU << "\n";
		exit(0);
	}

	if (CPU == "samplecg32I") {
		SampleCGArchVersion = SampleCG32I;
	}
	else if (CPU == "samplecg32II") {
		SampleCGArchVersion = SampleCG32II;
	}

	if (isSampleCG32I()) {
		HasCmp = true;
		HasSlt = false;
	}
	else if (isSampleCG32II()) {
		HasCmp = true;
		HasSlt = true;
	}
	else {
		errs() << "-mcpu must be empty (default:samplecg32II), samplecg32I or samplecg32II" << "\n";
	}
	ParseSubtargetFeatures(CPU, FS);
	InstrItins = getInstrItineraryForCPU(CPU);

	return *this;
}

bool SampleCGSubtarget::abiUsesSoftFloat() const {
  return true;
}

const SampleCGABIInfo &SampleCGSubtarget::getABI() const { return TM.getABI(); }
