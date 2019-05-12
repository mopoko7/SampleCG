//===-- SampleCGTargetMachine.cpp - Define TargetMachine for SampleCG -------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// Implements the info about SampleCG target spec.
//
//===----------------------------------------------------------------------===//

#include "SampleCGSEISelDAGToDAG.h"
#include "SampleCGTargetMachine.h"
#include "MCTargetDesc/SampleCGABIInfo.h"
#include "MCTargetDesc/SampleCGMCTargetDesc.h"
#include "SampleCG.h"
#include "SampleCGSubtarget.h"
#include "SampleCGTargetObjectFile.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Analysis/TargetTransformInfo.h"
#include "llvm/CodeGen/GlobalISel/IRTranslator.h"
#include "llvm/CodeGen/GlobalISel/Legalizer.h"
#include "llvm/CodeGen/GlobalISel/RegBankSelect.h"
#include "llvm/CodeGen/GlobalISel/InstructionSelect.h"
#include "llvm/CodeGen/BasicTTIImpl.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/CodeGen/TargetPassConfig.h"
#include "llvm/IR/Attributes.h"
#include "llvm/IR/Function.h"
#include "llvm/Support/CodeGen.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetOptions.h"
#include <string>

using namespace llvm;

#define DEBUG_TYPE "samplecg"

extern "C" void LLVMInitializeSampleCGTarget() {
  // Register the target.
  RegisterTargetMachine<SampleCGebTargetMachine> X(getTheSampleCGTarget());
  RegisterTargetMachine<SampleCGelTargetMachine> Y(getTheSampleCGelTarget());
}

static std::string computeDataLayout(const Triple &TT, StringRef CPU,
                                     const TargetOptions &Options,
                                     bool isLittle) {
  std::string Ret;

  // There are both little and big endian samplecg.
  if (isLittle)
    Ret += "e";
  else
    Ret += "E";

	Ret += "-m:m";

    Ret += "-p:32:32";

  // 8 and 16 bit integers only need to have natural alignment, but try to
  // align them to 32 bits. 64 bit integers have natural alignment.
  Ret += "-i8:8:32-i16:16:32-i64:64";

  // 32 bit registers are always available and the stack is at least 64 bit
  // aligned. On N64 64 bit registers are also available and the stack is
  // 128 bit aligned.
    Ret += "-n32-S64";

  return Ret;
}

static Reloc::Model getEffectiveRelocModel(bool JIT,
                                           Optional<Reloc::Model> RM) {
  if (!RM.hasValue() || JIT)
    return Reloc::Static;
  return *RM;
}

// On function prologue, the stack is created by decrementing
// its pointer. Once decremented, all references are done with positive
// offset from the stack/frame pointer, using StackGrowsUp enables
// an easier handling.
// Using CodeModel::Large enables different CALL behavior.
SampleCGTargetMachine::SampleCGTargetMachine(const Target &T, const Triple &TT,
                                     StringRef CPU, StringRef FS,
                                     const TargetOptions &Options,
                                     Optional<Reloc::Model> RM,
                                     Optional<CodeModel::Model> CM,
                                     CodeGenOpt::Level OL, bool JIT,
                                     bool isLittle)
    : LLVMTargetMachine(T, computeDataLayout(TT, CPU, Options, isLittle), TT,
                        CPU, FS, Options, getEffectiveRelocModel(JIT, RM),
                        getEffectiveCodeModel(CM, CodeModel::Small), OL),
      isLittle(isLittle), TLOF(llvm::make_unique<SampleCGTargetObjectFile>()),
      ABI(SampleCGABIInfo::computeTargetABI(TT, CPU, Options.MCOptions)),
      DefaultSubtarget(TT, CPU, FS, isLittle, *this,
                                           Options.StackAlignmentOverride) {
  initAsmInfo();
}

SampleCGTargetMachine::~SampleCGTargetMachine() = default;

void SampleCGebTargetMachine::anchor() {}

SampleCGebTargetMachine::SampleCGebTargetMachine(const Target &T, const Triple &TT,
                                         StringRef CPU, StringRef FS,
                                         const TargetOptions &Options,
                                         Optional<Reloc::Model> RM,
                                         Optional<CodeModel::Model> CM,
                                         CodeGenOpt::Level OL, bool JIT)
    : SampleCGTargetMachine(T, TT, CPU, FS, Options, RM, CM, OL, JIT, false) {}

void SampleCGelTargetMachine::anchor() {}

SampleCGelTargetMachine::SampleCGelTargetMachine(const Target &T, const Triple &TT,
                                         StringRef CPU, StringRef FS,
                                         const TargetOptions &Options,
                                         Optional<Reloc::Model> RM,
                                         Optional<CodeModel::Model> CM,
                                         CodeGenOpt::Level OL, bool JIT)
    : SampleCGTargetMachine(T, TT, CPU, FS, Options, RM, CM, OL, JIT, true) {}

const SampleCGSubtarget *
SampleCGTargetMachine::getSubtargetImpl(const Function &F) const {
  Attribute CPUAttr = F.getFnAttribute("target-cpu");
  Attribute FSAttr = F.getFnAttribute("target-features");

  std::string CPU = !CPUAttr.hasAttribute(Attribute::None)
                        ? CPUAttr.getValueAsString().str()
                        : TargetCPU;
  std::string FS = !FSAttr.hasAttribute(Attribute::None)
                       ? FSAttr.getValueAsString().str()
                       : TargetFS;

  auto &I = SubtargetMap[CPU + FS];
  if (!I) {
    // This needs to be done before we create a new subtarget since any
    // creation will depend on the TM and the code generation flags on the
    // function that reside in TargetOptions.
    resetTargetOptions(F);
    I = llvm::make_unique<SampleCGSubtarget>(TargetTriple, CPU, FS, isLittle, *this,
                                         Options.StackAlignmentOverride);
  }
  return I.get();
}

namespace {

/// SampleCG Code Generator Pass Configuration Options.
class SampleCGPassConfig : public TargetPassConfig {
public:
  SampleCGPassConfig(SampleCGTargetMachine &TM, PassManagerBase &PM)
      : TargetPassConfig(TM, PM) {
    // The current implementation of long branch pass requires a scratch
    // register ($at) to be available before branch instructions. Tail merging
    // can break this requirement, so disable it when long branch pass is
    // enabled.
    EnableTailMerge = !getSampleCGSubtarget().enableLongBranchPass();
  }

  SampleCGTargetMachine &getSampleCGTargetMachine() const {
    return getTM<SampleCGTargetMachine>();
  }

  const SampleCGSubtarget &getSampleCGSubtarget() const {
    return *getSampleCGTargetMachine().getSubtargetImpl();
  }

  void addIRPasses() override;
  bool addInstSelector() override;
  void addPreEmitPass() override;
#ifdef ENABLE_GPRESTORE
  void addPreRegAlloc() override;
#endif

};

} // end anonymous namespace

TargetPassConfig *SampleCGTargetMachine::createPassConfig(PassManagerBase &PM) {
  return new SampleCGPassConfig(*this, PM);
}

void SampleCGPassConfig::addIRPasses() {
  TargetPassConfig::addIRPasses();
  addPass(createAtomicExpandPass());
}

// Install an instruction selector pass using
// the ISelDag to gen Mips code.
bool SampleCGPassConfig::addInstSelector() {
  addPass(createSampleCGSEISelDag(getSampleCGTargetMachine(), getOptLevel()));
  return false;
}

#ifdef ENABLE_GPRESTORE
void SampleCGPassConfig::addPreRegAlloc() {
  if (!SampleCGReserveGP) {
    // $gp is a caller-saved register.
    addPass(createSampleCGEmitGPRestorePass(getSampleCGTargetMachine()));
  }
  return;
}
#endif

// Implemented by targets that want to run passes immediately before
// machine codeisemitted.returntrueif-print-machineinstrs should
// print out the code after the passes.
void SampleCGPassConfig::addPreEmitPass() {
	SampleCGTargetMachine &TM = getSampleCGTargetMachine();
	addPass(createSampleCGLongBranchPass(TM));
	addPass(createSampleCGDelJmpPass(TM));
	addPass(createSampleCGDelaySlotFillerPass(TM));
	return;
}
