//===-- llvm/Target/SampleCGTargetObjectFile.h - SampleCG Object Info ---*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_SAMPLECG_SAMPLECGTARGETOBJECTFILE_H
#define LLVM_LIB_TARGET_SAMPLECG_SAMPLECGTARGETOBJECTFILE_H

#include "llvm/CodeGen/TargetLoweringObjectFileImpl.h"

namespace llvm {
class SampleCGTargetMachine;
  class SampleCGTargetObjectFile : public TargetLoweringObjectFileELF {
    MCSection *SmallDataSection;
    MCSection *SmallBSSSection;
    const SampleCGTargetMachine *TM;
  public:
    void Initialize(MCContext &Ctx, const TargetMachine &TM) override;

	bool IsGlobalInSmallSection(const GlobalObject *GO,
								const TargetMachine &TM, SectionKind Kind) const;

	bool IsGlobalInSmallSection(const GlobalObject *GO,
								const TargetMachine &TM) const;

	bool IsGlobalInSmallSectionImpl(const GlobalValue*GV,
								const TargetMachine &TM) const;

	MCSection *SelectSectionForGlobal(const GlobalObject *GO, SectionKind Kind,
                                      const TargetMachine &TM) const override;

  };
} // end namespace llvm

#endif
