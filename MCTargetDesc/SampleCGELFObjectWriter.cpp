//===-- SampleCGELFObjectWriter.cpp - SampleCG ELF Writer -------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/SampleCGFixupKinds.h"
#include "MCTargetDesc/SampleCGMCTargetDesc.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/BinaryFormat/ELF.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCELFObjectWriter.h"
#include "llvm/MC/MCFixup.h"
#include "llvm/MC/MCObjectWriter.h"
#include "llvm/MC/MCSymbolELF.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/MathExtras.h"
#include "llvm/Support/raw_ostream.h"
#include <algorithm>
#include <cassert>
#include <cstdint>
#include <iterator>
#include <list>
#include <utility>

#define DEBUG_TYPE "samplecg-elf-object-writer"

using namespace llvm;

class SampleCGELFObjectWriter : public MCELFObjectTargetWriter {
public:
  SampleCGELFObjectWriter(uint8_t OSABI);

  ~SampleCGELFObjectWriter() override = default;

  unsigned getRelocType(MCContext &Ctx, const MCValue &Target,
                        const MCFixup &Fixup, bool IsPCRel) const override;
  bool needsRelocateWithSymbol(const MCSymbol &Sym,
                               unsigned Type) const override;
};

SampleCGELFObjectWriter::SampleCGELFObjectWriter(uint8_t OSABI)
    : MCELFObjectTargetWriter(false, OSABI, ELF::EM_SAMPLECG, false) {}


//@GetRelocType {
unsigned SampleCGELFObjectWriter::getRelocType(MCContext &Ctx,
                                           const MCValue &Target,
                                           const MCFixup &Fixup,
                                           bool IsPCRel) const {
  // determine the type of the relocation
  unsigned Type = (unsigned)ELF::R_CPU0_NONE;
  unsigned Kind = (unsigned)Fixup.getKind();

  switch (Kind) {
  default:
    llvm_unreachable("invalid fixup kind!");
  case FK_Data_4:
    Type = ELF::R_CPU0_32;
    break;
  case FK_GPRel_4:
    Type = ELF::R_CPU0_GPREL32;
    break;
  case SampleCG::fixup_SampleCG_32:
    Type = ELF::R_CPU0_32;
    break;
  case SampleCG::fixup_SampleCG_GPREL16:
    Type = ELF::R_CPU0_GPREL16;
    break;
  case SampleCG::fixup_SampleCG_CALL16:
    Type = ELF::R_CPU0_CALL16;
    break;
  case SampleCG::fixup_SampleCG_GOT:
    Type = ELF::R_CPU0_GOT16;
    break;
  case SampleCG::fixup_SampleCG_HI16:
    Type = ELF::R_CPU0_HI16;
    break;
  case SampleCG::fixup_SampleCG_LO16:
    Type = ELF::R_CPU0_LO16;
    break;
  case SampleCG::fixup_SampleCG_TLSGD:
    Type = ELF::R_CPU0_TLS_GD;
    break;
  case SampleCG::fixup_SampleCG_GOTTPREL:
    Type = ELF::R_CPU0_TLS_GOTTPREL;
    break;
  case SampleCG::fixup_SampleCG_PC16:
    Type = ELF::R_CPU0_PC16;
    break;
  case SampleCG::fixup_SampleCG_PC24:
    Type = ELF::R_CPU0_PC24;
    break;
  case SampleCG::fixup_SampleCG_TP_HI:
    Type = ELF::R_CPU0_TLS_TP_HI16;
    break;
  case SampleCG::fixup_SampleCG_TP_LO:
    Type = ELF::R_CPU0_TLS_TP_LO16;
    break;
  case SampleCG::fixup_SampleCG_TLSLDM:
    Type = ELF::R_CPU0_TLS_LDM;
    break;
  case SampleCG::fixup_SampleCG_DTP_HI:
    Type = ELF::R_CPU0_TLS_DTP_HI16;
    break;
  case SampleCG::fixup_SampleCG_DTP_LO:
    Type = ELF::R_CPU0_TLS_DTP_LO16;
    break;
  case SampleCG::fixup_SampleCG_GOT_HI16:
    Type = ELF::R_CPU0_GOT_HI16;
    break;
  case SampleCG::fixup_SampleCG_GOT_LO16:
    Type = ELF::R_CPU0_GOT_LO16;
    break;
  }

  return Type;
}
//@GetRelocType }

bool SampleCGELFObjectWriter::needsRelocateWithSymbol(const MCSymbol &Sym,
                                                  unsigned Type) const {
  switch (Type) {
  default:
    errs() << Type << "\n";
    llvm_unreachable("Unexpected relocation");
    return true;

	case ELF::R_CPU0_GOT16:
		return true;

	case ELF::R_CPU0_HI16:
	case ELF::R_CPU0_LO16:
	case ELF::R_CPU0_32:
		return true;

	case ELF::R_CPU0_GPREL16:
		return false;
  }
}

/*
MCObjectWriter *llvm::createSampleCGELFObjectWriter(raw_pwrite_stream &OS,
												uint8_t OSABI, bool IsLittleEndian) {
	MCELFObjectTargetWriter *MOTW = new SampleCGELFObjectWriter(OSABI);
	return createELFObjectWriter(MOTW, OS, IsLittleEndian);
}
*/

std::unique_ptr<MCObjectTargetWriter>
llvm::createSampleCGELFObjectWriter( uint8_t OSABI) {
  return llvm::make_unique<SampleCGELFObjectWriter>(OSABI);
}

