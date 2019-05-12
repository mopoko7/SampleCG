//===-- SampleCGTargetStreamer.h - SampleCG Target Streamer ------------*- C++ -*--===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_SAMPLECG_SAMPLECGTARGETSTREAMER_H
#define LLVM_LIB_TARGET_SAMPLECG_SAMPLECGTARGETSTREAMER_H

#include "MCTargetDesc/SampleCGABIInfo.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/MC/MCELFStreamer.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCStreamer.h"

namespace llvm {

struct SampleCGABIFlagsSection;

class SampleCGTargetStreamer : public MCTargetStreamer {
public:
  SampleCGTargetStreamer(MCStreamer &S);
  virtual void setPic(bool Value) {}

  void forbidModuleDirective() { ModuleDirectiveAllowed = false; }
  void reallowModuleDirective() { ModuleDirectiveAllowed = true; }
  bool isModuleDirectiveAllowed() { return ModuleDirectiveAllowed; }

  const SampleCGABIInfo &getABI() const {
    assert(ABI.hasValue() && "ABI hasn't been set!");
    return *ABI;
  }

protected:
  llvm::Optional<SampleCGABIInfo> ABI;
  bool GPRInfoSet;
  unsigned GPRBitMask;
  int GPROffset;

  bool FPRInfoSet;
  unsigned FPRBitMask;
  int FPROffset;

  bool FrameInfoSet;
  int FrameOffset;
  unsigned FrameReg;
  unsigned ReturnReg;

private:
  bool ModuleDirectiveAllowed;
};

// This part is for ascii assembly output
class SampleCGTargetAsmStreamer : public SampleCGTargetStreamer {
  formatted_raw_ostream &OS;

public:
  SampleCGTargetAsmStreamer(MCStreamer &S, formatted_raw_ostream &OS);
};

// This part is for ELF object output
class SampleCGTargetELFStreamer : public SampleCGTargetStreamer {
  bool MicroSampleCGEnabled;
  const MCSubtargetInfo &STI;
  bool Pic;

public:
  bool isMicroSampleCGEnabled() const { return MicroSampleCGEnabled; }
  MCELFStreamer &getStreamer();
  SampleCGTargetELFStreamer(MCStreamer &S, const MCSubtargetInfo &STI);
};
}
#endif
