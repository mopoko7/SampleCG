//===-- SampleCGMCCodeEmitter.cpp - Convert SampleCG Code to Machine Code ---------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file implements the SampleCGMCCodeEmitter class.
//
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/SampleCGBaseInfo.h"
#include "SampleCGMCCodeEmitter.h"
#include "MCTargetDesc/SampleCGFixupKinds.h"
#include "MCTargetDesc/SampleCGMCExpr.h"
#include "MCTargetDesc/SampleCGMCTargetDesc.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCFixup.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCInstrDesc.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
#include <cassert>
#include <cstdint>

using namespace llvm;

#define DEBUG_TYPE "mccodeemitter"

#define GET_INSTRMAP_INFO
#include "SampleCGGenInstrInfo.inc"
#undef GET_INSTRMAP_INFO

namespace llvm {

MCCodeEmitter *createSampleCGMCCodeEmitterEB(const MCInstrInfo &MCII,
                                         const MCRegisterInfo &MRI,
                                         MCContext &Ctx) {
  return new SampleCGMCCodeEmitter(MCII, Ctx, false);
}

MCCodeEmitter *createSampleCGMCCodeEmitterEL(const MCInstrInfo &MCII,
                                         const MCRegisterInfo &MRI,
                                         MCContext &Ctx) {
  return new SampleCGMCCodeEmitter(MCII, Ctx, true);
}

} // end namespace llvm

void SampleCGMCCodeEmitter::EmitByte(unsigned char C, raw_ostream &OS) const {
  OS << (char)C;
}

void SampleCGMCCodeEmitter::EmitInstruction(uint64_t Val, unsigned Size,
                                        const MCSubtargetInfo &STI,
                                        raw_ostream &OS) const {
	for (unsigned i = 0; i < Size; ++i) {
		unsigned Shift = IsLittleEndian ? i * 8 : (Size - 1 - i) * 8;
		EmitByte((Val >> Shift) & 0xff, OS);
	}
}

/// encodeInstruction - Emit the instruction.
/// Size the instruction with Desc.getSize().
void SampleCGMCCodeEmitter::
encodeInstruction(const MCInst &MI, raw_ostream &OS,
                  SmallVectorImpl<MCFixup> &Fixups,
                  const MCSubtargetInfo &STI) const
{
  uint32_t Binary = getBinaryCodeForInstr(MI, Fixups, STI);

  // Check for unimplemented opcodes.
  // Unfortunately in SAMPLECG both NOP and SLL will come in with Binary == 0
  // so we have to special check for them.
  unsigned Opcode = MI.getOpcode();
  if ((Opcode != SampleCG::NOP) && (Opcode != SampleCG::SHL) && !Binary)
    llvm_unreachable("unimplemented opcode in encodeInstruction()");

  const MCInstrDesc &Desc = MCII.get(MI.getOpcode());
  uint64_t TSFlags = Desc.TSFlags;

  if ((TSFlags & SampleCGII::FormMask) == SampleCGII::Pseudo) {
    llvm_unreachable("Pseudo opcode found in encodeInstruction()");
  }

  unsigned Size = Desc.getSize();
  if (!Size)
    llvm_unreachable("Desc.getSize() returns 0");

  EmitInstruction(Binary, Size, STI, OS);
}

/// getBranchTargetOpValue - Return binary encoding of the branch
/// target operand. If the machine operand requires relocation,
/// record the relocation and return zero.
unsigned SampleCGMCCodeEmitter::
getBranch16TargetOpValue(const MCInst &MI, unsigned OpNo,
                       SmallVectorImpl<MCFixup> &Fixups,
                       const MCSubtargetInfo &STI) const {
  return 0;
}

unsigned SampleCGMCCodeEmitter::
getBranch24TargetOpValue(const MCInst &MI, unsigned OpNo,
                       SmallVectorImpl<MCFixup> &Fixups,
                       const MCSubtargetInfo &STI) const {
  return 0;
}

/// getJumpTargetOpValue - Return binary encoding of the jump
/// target operand, such as JSUB.
/// If the machine operand requires relocation,
/// record the relocation and return zero.
//@getJumpTargetOpValue {
unsigned SampleCGMCCodeEmitter::
getJumpTargetOpValue(const MCInst &MI, unsigned OpNo,
                     SmallVectorImpl<MCFixup> &Fixups,
                     const MCSubtargetInfo &STI) const {
  unsigned Opcode = MI.getOpcode();
  const MCOperand &MO = MI.getOperand(OpNo);
  // If the destination is an immediate, we have nothing to do.
  if (MO.isImm()) return MO.getImm();
  assert(MO.isExpr() && "getJumpTargetOpValue expects only expressions");

  const MCExpr *Expr = MO.getExpr();
  if (Opcode == SampleCG::JSUB || Opcode == SampleCG::JMP || Opcode == SampleCG::BAL)
    Fixups.push_back(MCFixup::create(0, Expr,
                                     MCFixupKind(SampleCG::fixup_SampleCG_PC24)));
  else
    llvm_unreachable("unexpect opcode in getJumpAbsoluteTargetOpValue()");
  return 0;
}

//@getExprOpValue {
unsigned SampleCGMCCodeEmitter::
getExprOpValue(const MCExpr *Expr,SmallVectorImpl<MCFixup> &Fixups,
               const MCSubtargetInfo &STI) const {
//@getExprOpValue body {
  MCExpr::ExprKind Kind = Expr->getKind();
  if (Kind == MCExpr::Constant) {
    return cast<MCConstantExpr>(Expr)->getValue();
  }

  if (Kind == MCExpr::Binary) {
    unsigned Res = getExprOpValue(cast<MCBinaryExpr>(Expr)->getLHS(), Fixups, STI);
    Res += getExprOpValue(cast<MCBinaryExpr>(Expr)->getRHS(), Fixups, STI);
    return Res;
  }

  if (Kind == MCExpr::Target) {
    const SampleCGMCExpr *SampleCGExpr = cast<SampleCGMCExpr>(Expr);

    SampleCG::Fixups FixupKind = SampleCG::Fixups(0);
    switch (SampleCGExpr->getKind()) {
    default: llvm_unreachable("Unsupported fixup kind for target expression!");
  //@switch {
//    switch(cast<MCSymbolRefExpr>(Expr)->getKind()) {
  //@switch }
    case SampleCGMCExpr::CEK_GPREL:
      FixupKind = SampleCG::fixup_SampleCG_GPREL16;
      break;
    case SampleCGMCExpr::CEK_GOT_CALL:
      FixupKind = SampleCG::fixup_SampleCG_CALL16;
      break;
    case SampleCGMCExpr::CEK_GOT:
      FixupKind = SampleCG::fixup_SampleCG_GOT;
      break;
    case SampleCGMCExpr::CEK_ABS_HI:
      FixupKind = SampleCG::fixup_SampleCG_HI16;
      break;
    case SampleCGMCExpr::CEK_ABS_LO:
      FixupKind = SampleCG::fixup_SampleCG_LO16;
      break;
    case SampleCGMCExpr::CEK_TLSGD:
      FixupKind = SampleCG::fixup_SampleCG_TLSGD;
      break;
    case SampleCGMCExpr::CEK_TLSLDM:
      FixupKind = SampleCG::fixup_SampleCG_TLSLDM;
      break;
    case SampleCGMCExpr::CEK_DTP_HI:
      FixupKind = SampleCG::fixup_SampleCG_DTP_HI;
      break;
    case SampleCGMCExpr::CEK_DTP_LO:
      FixupKind = SampleCG::fixup_SampleCG_DTP_LO;
      break;
    case SampleCGMCExpr::CEK_GOTTPREL:
      FixupKind = SampleCG::fixup_SampleCG_GOTTPREL;
      break;
    case SampleCGMCExpr::CEK_TP_HI:
      FixupKind = SampleCG::fixup_SampleCG_TP_HI;
      break;
    case SampleCGMCExpr::CEK_TP_LO:
      FixupKind = SampleCG::fixup_SampleCG_TP_LO;
      break;
    case SampleCGMCExpr::CEK_GOT_HI16:
      FixupKind = SampleCG::fixup_SampleCG_GOT_HI16;
      break;
    case SampleCGMCExpr::CEK_GOT_LO16:
      FixupKind = SampleCG::fixup_SampleCG_GOT_LO16;
      break;
    } // switch
    Fixups.push_back(MCFixup::create(0, Expr, MCFixupKind(FixupKind)));
    return 0;
  }

  // All of the information is in the fixup.
  return 0;
}

/// getMachineOpValue - Return binary encoding of operand. If the machine
/// operand requires relocation, record the relocation and return zero.
unsigned SampleCGMCCodeEmitter::
getMachineOpValue(const MCInst &MI, const MCOperand &MO,
                  SmallVectorImpl<MCFixup> &Fixups,
                  const MCSubtargetInfo &STI) const {
  if (MO.isReg()) {
    unsigned Reg = MO.getReg();
    unsigned RegNo = Ctx.getRegisterInfo()->getEncodingValue(Reg);
    return RegNo;
  } else if (MO.isImm()) {
    return static_cast<unsigned>(MO.getImm());
  } else if (MO.isFPImm()) {
    return static_cast<unsigned>(APFloat(MO.getFPImm())
        .bitcastToAPInt().getHiBits(32).getLimitedValue());
  }
  // MO must be an Expr.
  assert(MO.isExpr());
  return getExprOpValue(MO.getExpr(),Fixups, STI);
}

/// Return binary encoding of memory related operand.
/// If the offset operand requires relocation, record the relocation.
template <unsigned ShiftAmount>
unsigned SampleCGMCCodeEmitter::getMemEncoding(const MCInst &MI, unsigned OpNo,
                                           SmallVectorImpl<MCFixup> &Fixups,
                                           const MCSubtargetInfo &STI) const {
  // Base register is encoded in bits 20-16, offset is encoded in bits 15-0.
  assert(MI.getOperand(OpNo).isReg());
  unsigned RegBits = getMachineOpValue(MI, MI.getOperand(OpNo),Fixups, STI) << 16;
  unsigned OffBits = getMachineOpValue(MI, MI.getOperand(OpNo+1), Fixups, STI);

  // Apply the scale factor if there is one.
  OffBits >>= ShiftAmount;

  return (OffBits & 0xFFFF) | RegBits;
}

#include "SampleCGGenMCCodeEmitter.inc"
