//===-- SampleCGTargetInfo.cpp - SampleCG Target Implementation -----------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "SampleCG.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/TargetRegistry.h"
using namespace llvm;

namespace llvm {
Target &getTheSampleCGTarget() {
  static Target TheSampleCGTarget;
  return TheSampleCGTarget;
}

Target &getTheSampleCGelTarget() {
  static Target TheSampleCGelTarget;
  return TheSampleCGelTarget;
}
}

extern "C" void LLVMInitializeSampleCGTargetInfo() {
  RegisterTarget<Triple::samplecg, /*HasJIT=*/true> X(getTheSampleCGTarget(), "samplecg",
                                                   "SampleCG", "SampleCG");
  RegisterTarget<Triple::samplecgel, /*HasJIT=*/true> Y(
      getTheSampleCGelTarget(), "samplecgel", "SampleCG LE", "SampleCG");
}

