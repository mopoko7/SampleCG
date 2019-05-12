//===-- SampleCGTargetObjectFile.cpp - SampleCG Object Files ----------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "SampleCGTargetObjectFile.h"
#include "SampleCGSubtarget.h"
#include "SampleCGTargetMachine.h"
#include "llvm/BinaryFormat/ELF.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCSectionELF.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Target/TargetMachine.h"
using namespace llvm;

static cl::opt<unsigned>
SSThreshold("samplecg-ssection-threshold", cl::Hidden,
            cl::desc("Small data and bss section threshold size (default=8)"),
            cl::init(8));

static cl::opt<bool>
LocalSData("mlocal-sdata", cl::Hidden,
           cl::desc("SAMPLECG: Use gp_rel for object-local data."),
           cl::init(true));

static cl::opt<bool>
ExternSData("mextern-sdata", cl::Hidden,
            cl::desc("SAMPLECG: Use gp_rel for data that is not defined by the "
                     "current object."),
            cl::init(true));

static cl::opt<bool>
EmbeddedData("membedded-data", cl::Hidden,
             cl::desc("SAMPLECG: Try to allocate variables in the following"
                      " sections if possible: .rodata, .sdata, .data ."),
             cl::init(false));

void SampleCGTargetObjectFile::Initialize(MCContext &Ctx, const TargetMachine &TM){
  TargetLoweringObjectFileELF::Initialize(Ctx, TM);
  InitializeELF(TM.Options.UseInitArray);

  SmallDataSection = getContext().getELFSection(
      ".sdata", ELF::SHT_PROGBITS,
      ELF::SHF_WRITE | ELF::SHF_ALLOC);

  SmallBSSSection = getContext().getELFSection(".sbss", ELF::SHT_NOBITS,
                                               ELF::SHF_WRITE | ELF::SHF_ALLOC);
  this->TM = &static_cast<const SampleCGTargetMachine &>(TM);
}

static bool IsInSmallSection(uint64_t Size) {
	return Size > 0 && Size <= SSThreshold;
}

bool SampleCGTargetObjectFile::IsGlobalInSmallSection(const GlobalObject *GO,
												const TargetMachine &TM) const {
	if (GO->isDeclaration() || GO->hasAvailableExternallyLinkage())
		return false;
	
	return IsGlobalInSmallSection(GO, TM, getKindForGlobal(GO, TM));
}

bool SampleCGTargetObjectFile::IsGlobalInSmallSection(const GlobalObject *GO,
				const TargetMachine &TM, SectionKind Kind) const {
	return (IsGlobalInSmallSectionImpl(GO, TM) &&
				(Kind.isData() || Kind.isBSS() || Kind.isCommon()));
}

bool SampleCGTargetObjectFile::IsGlobalInSmallSectionImpl(const GlobalValue*GV,
				const TargetMachine &TM) const {
	const SampleCGSubtarget &Subtarget =*static_cast<const SampleCGTargetMachine &>(TM).getSubtargetImpl();
	// Return if small section is not available.
	if (!Subtarget.useSmallSection())
		return false;
	
	// Only global variables, not functions.
	const GlobalVariable*GVA = dyn_cast<GlobalVariable>(GV);
	if (!GVA)
		return false;
	
	Type*Ty = GV->getValueType();
	return IsInSmallSection(
			GV->getParent()->getDataLayout().getTypeAllocSize(Ty));
}

MCSection *SampleCGTargetObjectFile::SelectSectionForGlobal(const GlobalObject *GO,
		SectionKind Kind, const TargetMachine &TM) const {
	// TODO: Could also support "weak" symbols as well with ".gnu.linkonce.s.*"
	// sections?
	
	// Handle Small Section classification here.
	if (Kind.isBSS() && IsGlobalInSmallSection(GO, TM, Kind))
		return SmallBSSSection;
	if (Kind.isData() && IsGlobalInSmallSection(GO, TM, Kind))
		return SmallDataSection;
	
	// Otherwise, we work the same as ELF.
	return TargetLoweringObjectFileELF::SelectSectionForGlobal(GO, Kind, TM);
}
