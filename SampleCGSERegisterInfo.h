//===-- SampleCGSERegisterInfo.h - SampleCG32/64 Register Information ---*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the SampleCG32/64 implementation of the TargetRegisterInfo
// class.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_SAMPLECG_SAMPLECGSEREGISTERINFO_H
#define LLVM_LIB_TARGET_SAMPLECG_SAMPLECGSEREGISTERINFO_H

#include "SampleCGRegisterInfo.h"

namespace llvm {

class SampleCGSERegisterInfo : public SampleCGRegisterInfo {
public:
  SampleCGSERegisterInfo(const SampleCGSubtarget &Subtarget);

  const TargetRegisterClass *intRegClass(unsigned Size) const override;
};

} // end namespace llvm

#endif
