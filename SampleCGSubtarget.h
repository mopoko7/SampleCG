//===-- SampleCGSubtarget.h - Define Subtarget for the SampleCG ---------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file declares the SampleCG specific subclass of TargetSubtargetInfo.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_SAMPLECG_SAMPLECGSUBTARGET_H
#define LLVM_LIB_TARGET_SAMPLECG_SAMPLECGSUBTARGET_H

#include "MCTargetDesc/SampleCGABIInfo.h"
#include "SampleCGFrameLowering.h"
#include "SampleCGISelLowering.h"
#include "SampleCGInstrInfo.h"
#include "llvm/CodeGen/SelectionDAGTargetInfo.h"
#include "llvm/CodeGen/TargetSubtargetInfo.h"
#include "llvm/CodeGen/GlobalISel/CallLowering.h"
#include "llvm/CodeGen/GlobalISel/LegalizerInfo.h"
#include "llvm/CodeGen/GlobalISel/RegisterBankInfo.h"
#include "llvm/CodeGen/GlobalISel/InstructionSelector.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/MC/MCInstrItineraries.h"
#include "llvm/Support/ErrorHandling.h"
#include <string>

#define GET_SUBTARGETINFO_HEADER
#include "SampleCGGenSubtargetInfo.inc"

extern bool SampleCGReserveGP;

extern bool SampleCGNoCpload;

namespace llvm {
class StringRef;

class SampleCGTargetMachine;

class SampleCGSubtarget : public SampleCGGenSubtargetInfo {
  virtual void anchor();

  enum SampleCGArchEnum {
    SampleCGDefault,
    SampleCG32I,
    SampleCG32II
  };

  // SampleCG architecture version
  SampleCGArchEnum SampleCGArchVersion;

  // IsLittle - The target is Little Endian
  bool IsLittle;

  bool EnableOverflow;

  bool HasCmp;

  bool HasSlt;

  bool UseSmallSection;

  InstrItineraryData InstrItins;

  const SampleCGTargetMachine &TM;

  Triple TargetTriple;

  const SelectionDAGTargetInfo TSInfo;

  std::unique_ptr<const SampleCGInstrInfo> InstrInfo;
  std::unique_ptr<const SampleCGFrameLowering> FrameLowering;
  std::unique_ptr<const SampleCGTargetLowering> TLInfo;

public:
  bool isPositionIndependent() const;
  const SampleCGABIInfo &getABI() const;

  /// This constructor initializes the data members to match that
  /// of the specified triple.
  SampleCGSubtarget(const Triple &TT, StringRef CPU, StringRef FS, bool little,
                const SampleCGTargetMachine &TM, unsigned StackAlignOverride);

  /// ParseSubtargetFeatures - Parses features string setting specified
  /// subtarget options.  Definition of function is auto generated by tblgen.
  void ParseSubtargetFeatures(StringRef CPU, StringRef FS);

  bool isLittle() const { return IsLittle; }
  bool hasSampleCG32I() const { return SampleCGArchVersion >= SampleCG32I; }
  bool isSampleCG32I() const { return SampleCGArchVersion == SampleCG32I; }
  bool hasSampleCG32II() const { return SampleCGArchVersion >= SampleCG32II; }
  bool isSampleCG32II() const { return SampleCGArchVersion == SampleCG32II; }

  bool enableOverflow() const { return EnableOverflow; }
  bool disableOverflow() const { return !EnableOverflow; }
  bool hasCmp() const { return HasCmp; }
  bool hasSlt() const { return HasSlt; }
  bool useSmallSection() const { return UseSmallSection; }

  bool abiUsesSoftFloat() const;

  bool enableLongBranchPass() const { return hasSampleCG32II(); }

  unsigned getStackAlignment() const { return 8; }

  // Grab relocation model
  Reloc::Model getRelocationModel() const;

  SampleCGSubtarget &initializeSubtargetDependencies(StringRef CPU, StringRef FS,
                                                 const TargetMachine &TM);

  const SampleCGInstrInfo *getInstrInfo() const override { return InstrInfo.get(); }
  const TargetFrameLowering *getFrameLowering() const override { return FrameLowering.get(); }
  const SampleCGRegisterInfo *getRegisterInfo() const override { return &InstrInfo->getRegisterInfo(); }
  const SampleCGTargetLowering *getTargetLowering() const override { return TLInfo.get(); }
  const InstrItineraryData *getInstrItineraryData() const override { return &InstrItins; }
};
} // End llvm namespace

#endif
