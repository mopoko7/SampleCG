//===-- SampleCGAsmBackend.cpp - SampleCG Asm Backend  ----------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file implements the SampleCGAsmBackend class.
//
//===----------------------------------------------------------------------===//
//

#include "MCTargetDesc/SampleCGAsmBackend.h"
#include "MCTargetDesc/SampleCGABIInfo.h"
#include "MCTargetDesc/SampleCGFixupKinds.h"
#include "MCTargetDesc/SampleCGMCExpr.h"
#include "MCTargetDesc/SampleCGMCTargetDesc.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/MC/MCAsmBackend.h"
#include "llvm/MC/MCAssembler.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCDirectives.h"
#include "llvm/MC/MCELFObjectWriter.h"
#include "llvm/MC/MCFixupKindInfo.h"
#include "llvm/MC/MCObjectWriter.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/MCTargetOptions.h"
#include "llvm/MC/MCValue.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/Format.h"
#include "llvm/Support/MathExtras.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

// Prepare value for the target space for it
static unsigned adjustFixupValue(const MCFixup &Fixup, uint64_t Value) {

  unsigned Kind = Fixup.getKind();

  // Add/subtract and shift
  switch (Kind) {
  default:
    return 0;
  case FK_DTPRel_4:
  case FK_Data_4:
  case FK_Data_8:
  case SampleCG::fixup_SampleCG_CALL16:
  case SampleCG::fixup_SampleCG_LO16:
    break;
  case SampleCG::fixup_SampleCG_HI16:
  case SampleCG::fixup_SampleCG_GOT:
    // Get the 2nd 16-bits. Also add 1 if bit 15 is 1.
    Value = ((Value + 0x8000) >> 16) & 0xffff;
    break;
  }

  return Value;
}

std::unique_ptr<MCObjectTargetWriter>
SampleCGAsmBackend::createObjectTargetWriter() const {
	return createSampleCGELFObjectWriter(MCELFObjectTargetWriter::getOSABI(OSType));
}

/*
MCObjectWriter *
SampleCGAsmBackend::createObjectWriter(raw_pwrite_stream &OS) const {
	return createSampleCGELFObjectWriter(OS, MCELFObjectTargetWriter::getOSABI(OSType), IsLittle);
}
*/

/// ApplyFixup - Apply the \p Value for given \p Fixup into the provided
/// data fragment, at the offset specified by the fixup and following the
/// fixup kind as appropriate.
void SampleCGAsmBackend::applyFixup(const MCAssembler &Asm, const MCFixup &Fixup,
                                const MCValue &Target,
                                MutableArrayRef<char> Data, uint64_t Value,
                                bool IsResolved,
                                const MCSubtargetInfo *STI) const {
  MCFixupKind Kind = Fixup.getKind();
  Value = adjustFixupValue(Fixup, Value);

  if (!Value)
    return; // Doesn't change encoding.

  // Where do we start in the object
  unsigned Offset = Fixup.getOffset();
  // Number of bytes we need to fixup
  unsigned NumBytes = (getFixupKindInfo(Kind).TargetSize + 7) / 8;
  // Used to point to big endian bytes
  unsigned FullSize;

  switch ((unsigned)Kind) {
  default:
    FullSize = 4;
    break;
  }

  // Grab current value, if any, from bits.
  uint64_t CurVal = 0;

  for (unsigned i = 0; i != NumBytes; ++i) {
    unsigned Idx = IsLittle ? i : (FullSize - 1 - i);
    CurVal |= (uint64_t)((uint8_t)Data[Offset + Idx]) << (i*8);
  }

  uint64_t Mask = ((uint64_t)(-1) >>
                    (64 - getFixupKindInfo(Kind).TargetSize));
  CurVal |= Value & Mask;

  // Write out the fixed up bytes back to the code/data bits.
  for (unsigned i = 0; i != NumBytes; ++i) {
    unsigned Idx = IsLittle ? i : (FullSize - 1 - i);
    Data[Offset + Idx] = (uint8_t)((CurVal >> (i*8)) & 0xff);
  }
}


const MCFixupKindInfo &SampleCGAsmBackend::
getFixupKindInfo(MCFixupKind Kind) const {
  const static MCFixupKindInfo Infos[SampleCG::NumTargetFixupKinds] = {
    // This table *must* be in same the order of fixup_* kinds in
    // SampleCGFixupKinds.h.
    //
    // name                        offset  bits  flags
    { "fixup_SampleCG_32",             0,     32,   0 },
    { "fixup_SampleCG_HI16",           0,     16,   0 },
    { "fixup_SampleCG_LO16",           0,     16,   0 },
    { "fixup_SampleCG_GPREL16",        0,     16,   0 },
    { "fixup_SampleCG_GOT",            0,     16,   0 },
    { "fixup_SampleCG_PC16",           0,     16,  MCFixupKindInfo::FKF_IsPCRel },
    { "fixup_SampleCG_PC24",           0,     24,  MCFixupKindInfo::FKF_IsPCRel },
    { "fixup_SampleCG_CALL16",         0,     16,   0 },
    { "fixup_SampleCG_TLSGD",          0,     16,   0 },
    { "fixup_SampleCG_GOTTP",          0,     16,   0 },
    { "fixup_SampleCG_TP_HI",          0,     16,   0 },
    { "fixup_SampleCG_TP_LO",          0,     16,   0 },
    { "fixup_SampleCG_TLSLDM",         0,     16,   0 },
    { "fixup_SampleCG_DTP_HI",         0,     16,   0 },
    { "fixup_SampleCG_DTP_LO",         0,     16,   0 },
    { "fixup_SampleCG_GOT_HI16",       0,     16,   0 },
    { "fixup_SampleCG_GOT_LO16",       0,     16,   0 }
  };

  if (Kind < FirstTargetFixupKind)
    return MCAsmBackend::getFixupKindInfo(Kind);

  assert(unsigned(Kind - FirstTargetFixupKind) < getNumFixupKinds() &&
         "Invalid kind!");
  return Infos[Kind - FirstTargetFixupKind];
}

/// WriteNopData - Write an (optimal) nop sequence of Count bytes
/// to the given output. If the target cannot generate such a sequence,
/// it should return an error.
///
/// \return - True on success.
bool SampleCGAsmBackend::writeNopData(raw_ostream &OS, uint64_t Count) const {
  // Check for a less than instruction size number of bytes
  // FIXME: 16 bit instructions are not handled yet here.
  // We shouldn't be using a hard coded number for instruction size.

  // If the count is not 4-byte aligned, we must be writing data into the text
  // section (otherwise we have unaligned instructions, and thus have far
  // bigger problems), so just write zeros instead.
  OS.write_zeros(Count);
  return true;
}

MCAsmBackend *llvm::createSampleCGAsmBackendEL32(const Target &T,
                                         const MCSubtargetInfo &STI,
                                         const MCRegisterInfo &MRI,
                                         const MCTargetOptions &Options) {
  return new SampleCGAsmBackend(T, STI.getTargetTriple().getOS(), false); 
}


MCAsmBackend *llvm::createSampleCGAsmBackendEB32(const Target &T,
                                         const MCSubtargetInfo &STI,
                                         const MCRegisterInfo &MRI,
                                         const MCTargetOptions &Options) {
  return new SampleCGAsmBackend(T, STI.getTargetTriple().getOS(), false); 
}

