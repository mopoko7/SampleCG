//===-- SampleCGMCTargetDesc.cpp - SampleCG Target Descriptions -----------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file provides SampleCG specific target descriptions.
//
//===----------------------------------------------------------------------===//

#include "SampleCGTargetStreamer.h"
#include "MCTargetDesc/SampleCGMCTargetDesc.h"
#include "InstPrinter/SampleCGInstPrinter.h"
#include "SampleCGMCAsmInfo.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/Triple.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCInstrAnalysis.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/TargetRegistry.h"
#include <cstdint>
#include <string>

#define GET_INSTRINFO_MC_DESC
#include "SampleCGGenInstrInfo.inc"

#define GET_SUBTARGETINFO_MC_DESC
#include "SampleCGGenSubtargetInfo.inc"

#define GET_REGINFO_MC_DESC
#include "SampleCGGenRegisterInfo.inc"

using namespace llvm;

//@1 {
/// Select the SampleCG Architecture Feature for the given triple and cpu name.
/// The function will be called at command 'llvm-objdump -d' for SampleCG elf input.
static StringRef selectSampleCGArchFeature(const Triple &TT, StringRef CPU) {
  std::string SampleCGArchFeature;
  if (CPU.empty() || CPU == "generic") {
    if (TT.getArch() == Triple::samplecg || TT.getArch() == Triple::samplecgel) {
      if (CPU.empty() || CPU == "samplecg32II") {
        SampleCGArchFeature = "+samplecg32II";
      }
      else {
        if (CPU == "samplecg32I") {
          SampleCGArchFeature = "+samplecg32I";
        }
      }
    }
  }
  return SampleCGArchFeature;
}
//@1 }

static MCInstrInfo *createSampleCGMCInstrInfo() {
  MCInstrInfo *X = new MCInstrInfo();
  InitSampleCGMCInstrInfo(X);
  return X;
}

static MCRegisterInfo *createSampleCGMCRegisterInfo(const Triple & /*TT*/) {
  MCRegisterInfo *X = new MCRegisterInfo();
  InitSampleCGMCRegisterInfo(X, SampleCG::SW, 0, 0, SampleCG::PC);
  return X;
}

static MCSubtargetInfo *
createSampleCGMCSubtargetInfo(const Triple &TT, StringRef CPU, StringRef FS) {
  std::string ArchFS = selectSampleCGArchFeature(TT,CPU);
  if (!FS.empty()) {
    if (!ArchFS.empty())
      ArchFS = ArchFS + "," + FS.str();
    else
      ArchFS = FS;
  }
  return createSampleCGMCSubtargetInfoImpl(TT, CPU, ArchFS);
// createSampleCGMCSubtargetInfoImpl defined in SampleCGGenSubtargetInfo.inc

}

static MCAsmInfo *createSampleCGMCAsmInfo(const MCRegisterInfo &MRI,
											const Triple &TT) {
	MCAsmInfo *MAI = new SampleCGMCAsmInfo(TT);
	unsigned SP = MRI.getDwarfRegNum(SampleCG::SP, true);
	MCCFIInstruction Inst = MCCFIInstruction::createDefCfa(nullptr, SP, 0);
	MAI->addInitialFrameState(Inst);

	return MAI;
}

static MCInstPrinter *createSampleCGMCInstPrinter(const Triple & /*T*/,
                                               unsigned SyntaxVariant,
                                               const MCAsmInfo &MAI,
                                               const MCInstrInfo &MII,
                                               const MCRegisterInfo &MRI) {
	SampleCGInstPrinter *IP = new SampleCGInstPrinter(MAI, MII, MRI);
	return IP;
}

namespace {

class SampleCGMCInstrAnalysis : public MCInstrAnalysis {
public:
  explicit SampleCGMCInstrAnalysis(const MCInstrInfo *Info)
      : MCInstrAnalysis(Info) {}

  bool evaluateBranch(const MCInst &Inst, uint64_t Addr, uint64_t Size,
                      uint64_t &Target) const override {
    if (Inst.getNumOperands() == 0)
      return false;

    if (Info->get(Inst.getOpcode()).OpInfo[0].OperandType ==
        MCOI::OPERAND_PCREL) {
      int64_t Imm = Inst.getOperand(0).getImm();
      Target = Addr + Size + Imm;
      return true;
    } else {
      int64_t Imm = Inst.getOperand(0).getImm();

      // Skip case where immediate is 0 as that occurs in file that isn't linked
      // and the branch target inferred would be wrong.
      if (Imm == 0)
        return false;

      Target = Imm;
      return true;
    }
  }
};

} // end anonymous namespace

static MCInstrAnalysis *createSampleCGMCInstrAnalysis(const MCInstrInfo *Info) {
  return new SampleCGMCInstrAnalysis(Info);
}

static MCStreamer *createMCStreamer(const Triple &T, MCContext &Context,
                                    std::unique_ptr<MCAsmBackend> &&MAB,
                                    std::unique_ptr<MCObjectWriter> &&OW,
                                    std::unique_ptr<MCCodeEmitter> &&Emitter,
                                    bool RelaxAll) {
	return createELFStreamer(Context, std::move(MAB), std::move(OW), std::move(Emitter), RelaxAll);
}

static MCTargetStreamer *createSampleCGAsmTargetStreamer(MCStreamer &S,
														formatted_raw_ostream &OS,
														MCInstPrinter*InstPrint,
														bool isVerboseAsm) {
	return new SampleCGTargetAsmStreamer(S, OS);
}

extern "C" void LLVMInitializeSampleCGTargetMC() {
	Target &TheSampleCGTarget = getTheSampleCGTarget();
	Target &TheSampleCGelTarget = getTheSampleCGelTarget();
	for (Target *T : {&TheSampleCGTarget, &TheSampleCGelTarget}) {
		RegisterMCAsmInfoFn X(*T, createSampleCGMCAsmInfo);

		TargetRegistry::RegisterMCInstrInfo(*T, createSampleCGMCInstrInfo);

		TargetRegistry::RegisterMCRegInfo(*T, createSampleCGMCRegisterInfo);

		TargetRegistry::RegisterMCSubtargetInfo(*T, createSampleCGMCSubtargetInfo);

		TargetRegistry::RegisterMCInstrAnalysis(*T, createSampleCGMCInstrAnalysis);

		TargetRegistry::RegisterMCInstPrinter(*T, createSampleCGMCInstPrinter);

		TargetRegistry::RegisterELFStreamer(*T, createMCStreamer);

		TargetRegistry::RegisterAsmTargetStreamer(*T, createSampleCGAsmTargetStreamer);

	}
	TargetRegistry::RegisterMCCodeEmitter(TheSampleCGTarget, createSampleCGMCCodeEmitterEB);

	TargetRegistry::RegisterMCCodeEmitter(TheSampleCGelTarget, createSampleCGMCCodeEmitterEL);

	TargetRegistry::RegisterMCAsmBackend(TheSampleCGTarget, createSampleCGAsmBackendEB32);

	TargetRegistry::RegisterMCAsmBackend(TheSampleCGelTarget, createSampleCGAsmBackendEL32);

}

