//===- SampleCGTargetMachine.h - Define TargetMachine for SampleCG ------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file declares the SampleCG specific subclass of TargetMachine.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_SAMPLECG_SAMPLECGTARGETMACHINE_H
#define LLVM_LIB_TARGET_SAMPLECG_SAMPLECGTARGETMACHINE_H

#include "MCTargetDesc/SampleCGABIInfo.h"
#include "SampleCGSubtarget.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/CodeGen.h"
#include "llvm/Target/TargetMachine.h"
#include <memory>

namespace llvm {

class SampleCGTargetMachine : public LLVMTargetMachine {
  bool isLittle;
  std::unique_ptr<TargetLoweringObjectFile> TLOF;
  // Selected ABI
  SampleCGABIInfo ABI;
  SampleCGSubtarget DefaultSubtarget;
  SampleCGSubtarget *Subtarget;

  mutable StringMap<std::unique_ptr<SampleCGSubtarget>> SubtargetMap;

public:
  SampleCGTargetMachine(const Target &T, const Triple &TT, StringRef CPU,
                    StringRef FS, const TargetOptions &Options,
                    Optional<Reloc::Model> RM, Optional<CodeModel::Model> CM,
                    CodeGenOpt::Level OL, bool JIT, bool isLittle);
  ~SampleCGTargetMachine() override;

  const SampleCGSubtarget *getSubtargetImpl() const {
    if (Subtarget)
      return Subtarget;
    return &DefaultSubtarget;
  }

  const SampleCGSubtarget *getSubtargetImpl(const Function &F) const override;

  // Pass Pipeline Configuration
  TargetPassConfig *createPassConfig(PassManagerBase &PM) override;

  TargetLoweringObjectFile *getObjFileLowering() const override {
    return TLOF.get();
  }

  bool isLittleEndian() const { return isLittle; }
  const SampleCGABIInfo &getABI() const { return ABI; }
};

/// SampleCG32/64 big endian target machine.
///
class SampleCGebTargetMachine : public SampleCGTargetMachine {
  virtual void anchor();

public:
  SampleCGebTargetMachine(const Target &T, const Triple &TT, StringRef CPU,
                      StringRef FS, const TargetOptions &Options,
                      Optional<Reloc::Model> RM, Optional<CodeModel::Model> CM,
                      CodeGenOpt::Level OL, bool JIT);
};

/// SampleCG32/64 little endian target machine.
///
class SampleCGelTargetMachine : public SampleCGTargetMachine {
  virtual void anchor();

public:
  SampleCGelTargetMachine(const Target &T, const Triple &TT, StringRef CPU,
                      StringRef FS, const TargetOptions &Options,
                      Optional<Reloc::Model> RM, Optional<CodeModel::Model> CM,
                      CodeGenOpt::Level OL, bool JIT);
};

} // end namespace llvm

#endif // LLVM_LIB_TARGET_SAMPLECG_SAMPLECGTARGETMACHINE_H
