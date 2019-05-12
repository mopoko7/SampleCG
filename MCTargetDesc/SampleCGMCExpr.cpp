//===-- SampleCGMCExpr.cpp - SampleCG specific MC expression classes --------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "SampleCGMCExpr.h"
#include "llvm/BinaryFormat/ELF.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCAssembler.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCSymbolELF.h"
#include "llvm/MC/MCValue.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/MathExtras.h"
#include "llvm/Support/raw_ostream.h"
#include <cstdint>

using namespace llvm;

#define DEBUG_TYPE "samplecgmcexpr"

const SampleCGMCExpr *SampleCGMCExpr::create(SampleCGMCExpr::SampleCGExprKind Kind,
                                     const MCExpr *Expr, MCContext &Ctx) {
  return new (Ctx) SampleCGMCExpr(Kind, Expr);
}

const SampleCGMCExpr*SampleCGMCExpr::create(const MCSymbol*Symbol,
							SampleCGMCExpr::SampleCGExprKind Kind, MCContext &Ctx) {
	const MCSymbolRefExpr*MCSym =
		MCSymbolRefExpr::create(Symbol, MCSymbolRefExpr::VK_None, Ctx);
	return new (Ctx) SampleCGMCExpr(Kind, MCSym);
}

const SampleCGMCExpr *SampleCGMCExpr::createGpOff(SampleCGMCExpr::SampleCGExprKind Kind,
                                          const MCExpr *Expr, MCContext &Ctx) {
  return create(Kind, create(CEK_None, create(CEK_GPREL, Expr, Ctx), Ctx), Ctx);
}

void SampleCGMCExpr::printImpl(raw_ostream &OS, const MCAsmInfo *MAI) const {
  int64_t AbsVal;

	switch (Kind) {
	case CEK_None:
	case CEK_Special:
		llvm_unreachable("CEK_None and CEK_Special are invalid");
		break;
	case CEK_CALL_HI16:
		OS << "%call_hi";
		break;
	case CEK_CALL_LO16:
		OS << "%call_lo";
		break;
	case CEK_DTP_HI:
		OS << "%dtp_hi";
		break;
	case CEK_DTP_LO:
		OS << "%dtp_lo";
		break;
	case CEK_GOT:
		OS << "%got";
		break;
	case CEK_GOTTPREL:
		OS << "%gottprel";
		break;
	case CEK_GOT_CALL:
		OS << "%call16";
		break;
	case CEK_GOT_DISP:
		OS << "%got_disp";
		break;
	case CEK_GOT_HI16:
		OS << "%got_hi";
		break;
	case CEK_GOT_LO16:
		OS << "%got_lo";
		break;
	case CEK_GPREL:
		OS << "%gp_rel";
		break;
	case CEK_ABS_HI:
		OS << "%hi";
		break;
	case CEK_ABS_LO:
		OS << "%lo";
		break;
	case CEK_TLSGD:
		OS << "%tlsgd";
		break;
	case CEK_TLSLDM:
		OS << "%tlsldm";
		break;
	case CEK_TP_HI:
		OS << "%tp_hi";
		break;
	case CEK_TP_LO:
		OS << "%tp_lo";
		break;
  }

  OS << '(';
  if (Expr->evaluateAsAbsolute(AbsVal))
    OS << AbsVal;
  else
    Expr->print(OS, MAI, true);
  OS << ')';
}

bool
SampleCGMCExpr::evaluateAsRelocatableImpl(MCValue &Res,
                                      const MCAsmLayout *Layout,
                                      const MCFixup *Fixup) const {
    return getSubExpr()->evaluateAsRelocatable(Res, Layout, Fixup);
}

void SampleCGMCExpr::visitUsedExpr(MCStreamer &Streamer) const {
  Streamer.visitUsedExpr(*getSubExpr());
}

void SampleCGMCExpr::fixELFSymbolsInTLSFixups(MCAssembler &Asm) const {
	switch (getKind()) {
	case CEK_None:
	case CEK_Special:
		llvm_unreachable("CEK_None and CEK_Special are invalid");
		break;
	case CEK_CALL_HI16:
	case CEK_CALL_LO16:
		break;
	default:
		break;
	}
}

bool SampleCGMCExpr::isGpOff(SampleCGExprKind &Kind) const {
  if (const SampleCGMCExpr *S1 = dyn_cast<const SampleCGMCExpr>(getSubExpr())) {
    if (const SampleCGMCExpr *S2 = dyn_cast<const SampleCGMCExpr>(S1->getSubExpr())) {
      if (S1->getKind() == CEK_None && S2->getKind() == CEK_GPREL) {
        Kind = getKind();
        return true;
      }
    }
  }
  return false;
}
