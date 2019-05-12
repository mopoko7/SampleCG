//===- SampleCGMCInstLower.cpp - Convert SampleCG MachineInstr to MCInst ----------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains code to lower SampleCG MachineInstrs to their corresponding
// MCInst records.
//
//===----------------------------------------------------------------------===//

#include "SampleCGAsmPrinter.h"
#include "MCTargetDesc/SampleCGBaseInfo.h"
#include "SampleCGMCInstLower.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineOperand.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInst.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/MC/MCContext.h"
#include <cassert>

using namespace llvm;

SampleCGMCInstLower::SampleCGMCInstLower(SampleCGAsmPrinter &asmprinter)
  : AsmPrinter(asmprinter) {}

void SampleCGMCInstLower::Initialize(MCContext *C) {
  Ctx = C;
}

//@LowerSymbolOperand {
MCOperand SampleCGMCInstLower::LowerSymbolOperand(const MachineOperand &MO,
                                              MachineOperandType MOTy,
                                              unsigned Offset) const {
  MCSymbolRefExpr::VariantKind Kind = MCSymbolRefExpr::VK_None;
  SampleCGMCExpr::SampleCGExprKind TargetKind = SampleCGMCExpr::CEK_None;
  const MCSymbol *Symbol;

  switch(MO.getTargetFlags()) {
  default:                   llvm_unreachable("Invalid target flag!");
  case SampleCGII::MO_NO_FLAG:
    break;

// SampleCG_GPREL is for llc -march=cpu0 -relocation-model=static -cpu0-islinux-
//  format=false (global var in .sdata).
  case SampleCGII::MO_GPREL:
    TargetKind = SampleCGMCExpr::CEK_GPREL;
    break;

  case SampleCGII::MO_GOT_CALL:
    TargetKind = SampleCGMCExpr::CEK_GOT_CALL;
    break;
  case SampleCGII::MO_GOT:
    TargetKind = SampleCGMCExpr::CEK_GOT;
    break;
// ABS_HI and ABS_LO is for llc -march=cpu0 -relocation-model=static (global 
//  var in .data).
  case SampleCGII::MO_ABS_HI:
    TargetKind = SampleCGMCExpr::CEK_ABS_HI;
    break;
  case SampleCGII::MO_ABS_LO:
    TargetKind = SampleCGMCExpr::CEK_ABS_LO;
    break;
  case SampleCGII::MO_TLSGD:
    TargetKind = SampleCGMCExpr::CEK_TLSGD;
    break;
  case SampleCGII::MO_TLSLDM:
    TargetKind = SampleCGMCExpr::CEK_TLSLDM;
    break;
  case SampleCGII::MO_DTP_HI:
    TargetKind = SampleCGMCExpr::CEK_DTP_HI;
    break;
  case SampleCGII::MO_DTP_LO:
    TargetKind = SampleCGMCExpr::CEK_DTP_LO;
    break;
  case SampleCGII::MO_GOTTPREL:
    TargetKind = SampleCGMCExpr::CEK_GOTTPREL;
    break;
  case SampleCGII::MO_TP_HI:
    TargetKind = SampleCGMCExpr::CEK_TP_HI;
    break;
  case SampleCGII::MO_TP_LO:
    TargetKind = SampleCGMCExpr::CEK_TP_LO;
    break;
  case SampleCGII::MO_GOT_HI16:
    TargetKind = SampleCGMCExpr::CEK_GOT_HI16;
    break;
  case SampleCGII::MO_GOT_LO16:
    TargetKind = SampleCGMCExpr::CEK_GOT_LO16;
    break;
  }

  switch (MOTy) {
  case MachineOperand::MO_GlobalAddress:
    Symbol = AsmPrinter.getSymbol(MO.getGlobal());
    Offset += MO.getOffset();
    break;

  case MachineOperand::MO_MachineBasicBlock:
    Symbol = MO.getMBB()->getSymbol();
    break;

  case MachineOperand::MO_BlockAddress:
    Symbol = AsmPrinter.GetBlockAddressSymbol(MO.getBlockAddress());
    Offset += MO.getOffset();
    break;

  case MachineOperand::MO_ExternalSymbol:
    Symbol = AsmPrinter.GetExternalSymbolSymbol(MO.getSymbolName());
    Offset += MO.getOffset();
    break;

  case MachineOperand::MO_JumpTableIndex:
    Symbol = AsmPrinter.GetJTISymbol(MO.getIndex());
    break;

  default:
    llvm_unreachable("<unknown operand type>");
  }

  const MCExpr *Expr = MCSymbolRefExpr::create(Symbol, Kind, *Ctx);

  if (Offset) {
    // Assume offset is never negative.
    assert(Offset > 0);
    Expr = MCBinaryExpr::createAdd(Expr, MCConstantExpr::create(Offset, *Ctx),
                                   *Ctx);
  }

  if (TargetKind != SampleCGMCExpr::CEK_None)
    Expr = SampleCGMCExpr::create(TargetKind, Expr, *Ctx);

  return MCOperand::createExpr(Expr);

}
//@LowerSymbolOperand }

#ifdef ENABLE_GPRESTORE
// Lower ".cprestore offset" to "st $gp, offset($sp)".
void SampleCGMCInstLower::LowerCPRESTORE(int64_t Offset,
                                     SmallVector<MCInst, 4>& MCInsts) {
  assert(isInt<32>(Offset) && (Offset >= 0) &&
         "Imm operand of .cprestore must be a non-negative 32-bit value.");

  MCOperand SPReg = MCOperand::createReg(SampleCG::SP), BaseReg = SPReg;
  MCOperand GPReg = MCOperand::createReg(SampleCG::GP);
  MCOperand ZEROReg = MCOperand::createReg(SampleCG::ZERO);

  if (!isInt<16>(Offset)) {
    unsigned Hi = ((Offset + 0x8000) >> 16) & 0xffff;
    Offset &= 0xffff;
    MCOperand ATReg = MCOperand::createReg(SampleCG::AT);
    BaseReg = ATReg;

    // lui   at,hi
    // add   at,at,sp
    MCInsts.resize(2);
    CreateMCInst(MCInsts[0], SampleCG::LUi, ATReg, ZEROReg, MCOperand::createImm(Hi));
    CreateMCInst(MCInsts[1], SampleCG::ADD, ATReg, ATReg, SPReg);
  }

  MCInst St;
  CreateMCInst(St, SampleCG::ST, GPReg, BaseReg, MCOperand::createImm(Offset));
  MCInsts.push_back(St);
}
#endif

MCOperand SampleCGMCInstLower::LowerOperand(const MachineOperand& MO,
                                        unsigned offset) const {
  MachineOperandType MOTy = MO.getType();

  switch (MOTy) {
  //@2
  default: llvm_unreachable("unknown operand type");
  case MachineOperand::MO_Register:
    // Ignore all implicit register operands.
    if (MO.isImplicit()) break;
    return MCOperand::createReg(MO.getReg());
  case MachineOperand::MO_Immediate:
    return MCOperand::createImm(MO.getImm() + offset);
  case MachineOperand::MO_MachineBasicBlock:
  case MachineOperand::MO_ExternalSymbol:
  case MachineOperand::MO_JumpTableIndex:
  case MachineOperand::MO_BlockAddress:
  case MachineOperand::MO_GlobalAddress:
//@1
    return LowerSymbolOperand(MO, MOTy, offset);
  case MachineOperand::MO_RegisterMask:
    break;
 }

  return MCOperand();
}

void SampleCGMCInstLower::Lower(const MachineInstr *MI, MCInst &OutMI) const {
	if (lowerLongBranch(MI, OutMI))
		return;

  OutMI.setOpcode(MI->getOpcode());

  for (unsigned i = 0, e = MI->getNumOperands(); i != e; ++i) {
    const MachineOperand &MO = MI->getOperand(i);
    MCOperand MCOp = LowerOperand(MO);

    if (MCOp.isValid())
      OutMI.addOperand(MCOp);
  }
}

static void CreateMCInst(MCInst& Inst, unsigned Opc, const MCOperand& Opnd0,
							const MCOperand& Opnd1,const MCOperand& Opnd2 = MCOperand()) {
	Inst.setOpcode(Opc);
	Inst.addOperand(Opnd0);
	Inst.addOperand(Opnd1);
	if(Opnd2.isValid())
		Inst.addOperand(Opnd2);
}

void SampleCGMCInstLower::LowerCPLOAD(SmallVector<MCInst, 4>& MCInsts) {
	MCOperand GPReg = MCOperand::createReg(SampleCG::GP);
	MCOperand T9Reg = MCOperand::createReg(SampleCG::T9);
	StringRef SymName("_gp_disp");
	const MCSymbol *Sym = Ctx->getOrCreateSymbol(SymName);
	const SampleCGMCExpr *MCSym;
	MCSym = SampleCGMCExpr::create(Sym, SampleCGMCExpr::CEK_ABS_HI,*Ctx);
	MCOperand SymHi = MCOperand::createExpr(MCSym);
	MCSym = SampleCGMCExpr::create(Sym, SampleCGMCExpr::CEK_ABS_LO,*Ctx);
	MCOperand SymLo = MCOperand::createExpr(MCSym);
	MCInsts.resize(3);
	CreateMCInst(MCInsts[0], SampleCG::LUi, GPReg, SymHi);
	CreateMCInst(MCInsts[1], SampleCG::ORi, GPReg, GPReg, SymLo);
	CreateMCInst(MCInsts[2], SampleCG::ADD, GPReg, GPReg, T9Reg);
}

MCOperand SampleCGMCInstLower::createSub(MachineBasicBlock*BB1,
										MachineBasicBlock*BB2,
										SampleCGMCExpr::SampleCGExprKind Kind) const {
	const MCSymbolRefExpr*Sym1 = MCSymbolRefExpr::create(BB1->getSymbol(),*Ctx);
	const MCSymbolRefExpr*Sym2 = MCSymbolRefExpr::create(BB2->getSymbol(),*Ctx);
	const MCBinaryExpr*Sub = MCBinaryExpr::createSub(Sym1, Sym2,*Ctx);
	return MCOperand::createExpr(SampleCGMCExpr::create(Kind, Sub,*Ctx));
}
	
void SampleCGMCInstLower::lowerLongBranchLUi(const MachineInstr*MI, MCInst &OutMI) const {
	OutMI.setOpcode(SampleCG::LUi);
	
	// Lower register operand.
	OutMI.addOperand(LowerOperand(MI->getOperand(0)));
	
	// Create %hi($tgt-$baltgt).
	OutMI.addOperand(createSub(MI->getOperand(1).getMBB(),
								MI->getOperand(2).getMBB(),
								SampleCGMCExpr::CEK_ABS_HI));
}

void SampleCGMCInstLower::lowerLongBranchADDiu(const MachineInstr*MI, MCInst &OutMI, int Opcode,
											SampleCGMCExpr::SampleCGExprKind Kind) const {
	OutMI.setOpcode(Opcode);
	
	// Lower two register operands.
	for (unsigned I = 0, E = 2; I != E; ++I) {
		const MachineOperand &MO = MI->getOperand(I);
		OutMI.addOperand(LowerOperand(MO));
	}
	
	// Create %lo($tgt-$baltgt) or %hi($tgt-$baltgt).
	OutMI.addOperand(createSub(MI->getOperand(2).getMBB(),
								MI->getOperand(3).getMBB(), Kind));
}


bool SampleCGMCInstLower::lowerLongBranch(const MachineInstr*MI,
										MCInst &OutMI) const {
	switch (MI->getOpcode()) {
		default:return false;
	case SampleCG::LONG_BRANCH_LUi:
		lowerLongBranchLUi(MI, OutMI);
		return true;
	case SampleCG::LONG_BRANCH_ADDiu:
		lowerLongBranchADDiu(MI, OutMI, SampleCG::ADDiu,
								SampleCGMCExpr::CEK_ABS_LO);
		return true;
	}
}
