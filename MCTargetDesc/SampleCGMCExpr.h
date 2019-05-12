//===- SampleCGMCExpr.h - SampleCG specific MC expression classes -------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_SAMPLECG_MCTARGETDESC_SAMPLECGMCEXPR_H
#define LLVM_LIB_TARGET_SAMPLECG_MCTARGETDESC_SAMPLECGMCEXPR_H

#include "llvm/MC/MCAsmLayout.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCValue.h"

namespace llvm {

class SampleCGMCExpr : public MCTargetExpr {
public:
  enum SampleCGExprKind {
	  CEK_None,
	CEK_ABS_HI,
	CEK_ABS_LO,
	CEK_CALL_HI16,
	CEK_CALL_LO16,
	CEK_DTP_HI,
	CEK_DTP_LO,
	CEK_GOT,
	CEK_GOTTPREL,
	CEK_GOT_CALL,
	CEK_GOT_DISP,
	CEK_GOT_HI16,
	CEK_GOT_LO16,
	CEK_GPREL,
	CEK_TLSGD,
	CEK_TLSLDM,
	CEK_TP_HI,
	CEK_TP_LO,
	CEK_Special,
  };

private:
  const SampleCGExprKind Kind;
  const MCExpr *Expr;

  explicit SampleCGMCExpr(SampleCGExprKind Kind, const MCExpr *Expr)
      : Kind(Kind), Expr(Expr) {}

public:
  static const SampleCGMCExpr *create(SampleCGExprKind Kind, const MCExpr *Expr,
                                  MCContext &Ctx);
  static const SampleCGMCExpr *create(const MCSymbol *Symbol,
		  								SampleCGMCExpr::SampleCGExprKind Kind, MCContext &Ctx);
  static const SampleCGMCExpr *createGpOff(SampleCGExprKind Kind, const MCExpr *Expr,
                                       MCContext &Ctx);

  /// Get the kind of this expression.
  SampleCGExprKind getKind() const { return Kind; }

  /// Get the child of this expression.
  const MCExpr *getSubExpr() const { return Expr; }

  void printImpl(raw_ostream &OS, const MCAsmInfo *MAI) const override;
  bool evaluateAsRelocatableImpl(MCValue &Res, const MCAsmLayout *Layout,
                                 const MCFixup *Fixup) const override;
  void visitUsedExpr(MCStreamer &Streamer) const override;

  MCFragment *findAssociatedFragment() const override {
    return getSubExpr()->findAssociatedFragment();
  }

  void fixELFSymbolsInTLSFixups(MCAssembler &Asm) const override;

  static bool classof(const MCExpr *E) {
    return E->getKind() == MCExpr::Target;
  }

  bool isGpOff(SampleCGExprKind &Kind) const;
  bool isGpOff() const {
    SampleCGExprKind Kind;
    return isGpOff(Kind);
  }
};

} // end namespace llvm

#endif // LLVM_LIB_TARGET_SAMPLECG_MCTARGETDESC_SAMPLECGMCEXPR_H
