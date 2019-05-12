//===-- SampleCGTargetStreamer.cpp - SampleCG Target Streamer Methods -------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file provides SampleCG specific target streamer methods.
//
//===----------------------------------------------------------------------===//

#include "SampleCGTargetStreamer.h"
#include "InstPrinter/SampleCGInstPrinter.h"
#include "MCTargetDesc/SampleCGABIInfo.h"
#include "SampleCGMCExpr.h"
#include "SampleCGMCTargetDesc.h"
#include "SampleCGTargetObjectFile.h"
#include "llvm/BinaryFormat/ELF.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCSectionELF.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/MCSymbolELF.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/FormattedStream.h"

using namespace llvm;

namespace {
static cl::opt<bool> RoundSectionSizes(
    "samplecg-round-section-sizes", cl::init(false),
    cl::desc("Round section sizes up to the section alignment"), cl::Hidden);
} // end anonymous namespace

SampleCGTargetStreamer::SampleCGTargetStreamer(MCStreamer &S)
    : MCTargetStreamer(S), ModuleDirectiveAllowed(true) {
  GPRInfoSet = FPRInfoSet = FrameInfoSet = false;
}
SampleCGTargetAsmStreamer::SampleCGTargetAsmStreamer(MCStreamer &S,
                                             formatted_raw_ostream &OS)
    : SampleCGTargetStreamer(S), OS(OS) {}

// This part is for ELF object output.
SampleCGTargetELFStreamer::SampleCGTargetELFStreamer(MCStreamer &S,
                                             const MCSubtargetInfo &STI)
    : SampleCGTargetStreamer(S), MicroSampleCGEnabled(false), STI(STI) {
}

MCELFStreamer &SampleCGTargetELFStreamer::getStreamer() {
  return static_cast<MCELFStreamer &>(Streamer);
}
