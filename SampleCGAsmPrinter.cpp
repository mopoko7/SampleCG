//===- SampleCGAsmPrinter.cpp - SampleCG LLVM Assembly Printer --------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains a printer that converts from our internal representation
// of machine-dependent LLVM code to GAS-format SAMPLECG assembly language.
//
//===----------------------------------------------------------------------===//

#include "SampleCGAsmPrinter.h"
#include "InstPrinter/SampleCGInstPrinter.h"
#include "MCTargetDesc/SampleCGABIInfo.h"
#include "MCTargetDesc/SampleCGBaseInfo.h"
#include "MCTargetDesc/SampleCGMCTargetDesc.h"
#include "SampleCG.h"
#include "SampleCGMCInstLower.h"
#include "SampleCGMachineFunction.h"
#include "SampleCGSubtarget.h"
#include "SampleCGTargetMachine.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/Triple.h"
#include "llvm/ADT/Twine.h"
#include "llvm/BinaryFormat/ELF.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineConstantPool.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineJumpTableInfo.h"
#include "llvm/CodeGen/MachineOperand.h"
#include "llvm/CodeGen/TargetRegisterInfo.h"
#include "llvm/CodeGen/TargetSubtargetInfo.h"
#include "llvm/IR/Attributes.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/InlineAsm.h"
#include "llvm/IR/Instructions.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCInstBuilder.h"
#include "llvm/MC/MCObjectFileInfo.h"
#include "llvm/MC/MCSectionELF.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/MC/MCSymbolELF.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include <cassert>
#include <cstdint>
#include <map>
#include <memory>
#include <string>
#include <vector>

using namespace llvm;

#define DEBUG_TYPE "samplecg-asm-printer"

#ifdef ENABLE_GPRESTORE
void SampleCGAsmPrinter::EmitInstrWithMacroNoAT(const MachineInstr *MI) {
  MCInst TmpInst;

  MCInstLowering.Lower(MI, TmpInst);
  OutStreamer->EmitRawText(StringRef("\t.set\tmacro"));
  if (SampleCGFI->getEmitNOAT())
    OutStreamer->EmitRawText(StringRef("\t.set\tat"));
  OutStreamer->EmitInstruction(TmpInst, getSubtargetInfo());
  if (SampleCGFI->getEmitNOAT())
    OutStreamer->EmitRawText(StringRef("\t.set\tnoat"));
  OutStreamer->EmitRawText(StringRef("\t.set\tnomacro"));
}
#endif


bool SampleCGAsmPrinter::runOnMachineFunction(MachineFunction &MF) {
  SampleCGFI = MF.getInfo<SampleCGFunctionInfo>();
  AsmPrinter::runOnMachineFunction(MF);
  return true;
}

bool SampleCGAsmPrinter::lowerOperand(const MachineOperand &MO, MCOperand &MCOp) {
  MCOp = MCInstLowering.LowerOperand(MO);
  return MCOp.isValid();
}

#include "SampleCGGenMCPseudoLowering.inc"

#ifdef ENABLE_GPRESTORE
void SampleCGAsmPrinter::emitPseudoCPRestore(MCStreamer &OutStreamer,
                                              const MachineInstr *MI) {
  unsigned Opc = MI->getOpcode();
  SmallVector<MCInst, 4> MCInsts;
  const MachineOperand &MO = MI->getOperand(0);
  assert(MO.isImm() && "CPRESTORE's operand must be an immediate.");
  int64_t Offset = MO.getImm();

  if (OutStreamer.hasRawTextSupport()) {
    // output assembly
    if (!isInt<16>(Offset)) {
      EmitInstrWithMacroNoAT(MI);
      return;
    }
    MCInst TmpInst0;
    MCInstLowering.Lower(MI, TmpInst0);
    OutStreamer.EmitInstruction(TmpInst0, getSubtargetInfo());
  } else {
    // output elf
    MCInstLowering.LowerCPRESTORE(Offset, MCInsts);

    for (SmallVector<MCInst, 4>::iterator I = MCInsts.begin();
         I != MCInsts.end(); ++I)
      OutStreamer.EmitInstruction(*I, getSubtargetInfo());

    return;
  }
}
#endif

bool SampleCGAsmPrinter::isLongBranchPseudo(int Opcode) const {
	return(Opcode == SampleCG::LONG_BRANCH_LUi|| Opcode == SampleCG::LONG_BRANCH_ADDiu);
}

void SampleCGAsmPrinter::EmitInstruction(const MachineInstr *MI) {
  if (MI->isDebugValue()) {
    SmallString<128> Str;
    raw_svector_ostream OS(Str);

    PrintDebugValueComment(MI, OS);
    return;
  }

  MachineBasicBlock::const_instr_iterator I = MI->getIterator();
  MachineBasicBlock::const_instr_iterator E = MI->getParent()->instr_end();

  do {
	// Do any auto-generated pseudo lowerings.
    if (emitPseudoExpansionLowering(*OutStreamer, &*I))
      continue;

#ifdef ENABLE_GPRESTORE
    if (I->getOpcode() == SampleCG::CPRESTORE) {
      emitPseudoCPRestore(*OutStreamer, &*I);
      continue;
    }
#endif

	if (I->isPseudo() && !isLongBranchPseudo(I->getOpcode()))
      llvm_unreachable("Pseudo opcode found in EmitInstruction()");

    MCInst TmpInst0;
    MCInstLowering.Lower(&*I, TmpInst0);
    OutStreamer->EmitInstruction(TmpInst0, getSubtargetInfo());
  } while ((++I != E) && I->isInsideBundle()); // Delay slot check
}

//===----------------------------------------------------------------------===//
//
//  SampleCG Asm Directives
//
//  -- Frame directive "frame Stackpointer, Stacksize, RARegister"
//  Describe the stack frame.
//
//  -- Mask directives "(f)mask  bitmask, offset"
//  Tells the assembler which registers are saved and where.
//  bitmask - contain a little endian bitset indicating which registers are
//            saved on function prologue (e.g. with a 0x80000000 mask, the
//            assembler knows the register 31 (RA) is saved at prologue.
//  offset  - the position before stack pointer subtraction indicating where
//            the first saved register on prologue is located. (e.g. with a
//
//  Consider the following function prologue:
//
//    .frame  $fp,48,$ra
//    .mask   0xc0000000,-8
//       addiu $sp, $sp, -48
//       sw $ra, 40($sp)
//       sw $fp, 36($sp)
//
//    With a 0xc0000000 mask, the assembler knows the register 31 (RA) and
//    30 (FP) are saved at prologue. As the save order on prologue is from
//    left to right, RA is saved first. A -8 offset means that after the
//    stack pointer subtration, the first register in the mask (RA) will be
//    saved at address 48-8=40.
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Mask directives
//===----------------------------------------------------------------------===//

// Create a bitmask with all callee saved registers for CPU or Floating Point
// registers. For CPU registers consider RA, GP and FP for saving if necessary.
void SampleCGAsmPrinter::printSavedRegsBitmask(raw_ostream &O) {
  // CPU and FPU Saved Registers Bitmasks
  unsigned CPUBitmask = 0;
  int CPUTopSavedRegOff;

  // Set the CPU and FPU Bitmasks
  const MachineFrameInfo &MFI = MF->getFrameInfo();
  const TargetRegisterInfo *TRI = MF->getSubtarget().getRegisterInfo();
  const std::vector<CalleeSavedInfo> &CSI = MFI.getCalleeSavedInfo();
  // size of stack area to which FP callee-saved regs are saved.
  unsigned CPURegSize = TRI->getRegSizeInBits(SampleCG::CPURegsRegClass) / 8;

  for (const auto &I : CSI) {
    unsigned Reg = I.getReg();
    unsigned RegNum = TRI->getEncodingValue(Reg);
    CPUBitmask |= (1 << RegNum);
  }

  CPUTopSavedRegOff = CPUBitmask ? -CPURegSize : 0;

  O << "\t.mask \t";
  printHex32(CPUBitmask, O);
  O << "," << CPUTopSavedRegOff << "\n";
}

void SampleCGAsmPrinter::printHex32(unsigned Value, raw_ostream &O) {
	O << "0x";
	for (int i = 7; i >= 0; i--) {
		O.write_hex((Value & (0xF << (i*4))) >> (i*4));
	}
}

//===----------------------------------------------------------------------===//
// Frame and Set directives
//===----------------------------------------------------------------------===//

/// Frame Directive
void SampleCGAsmPrinter::emitFrameDirective() {
  const TargetRegisterInfo &RI = *MF->getSubtarget().getRegisterInfo();

  unsigned stackReg  = RI.getFrameRegister(*MF);
  unsigned returnReg = RI.getRARegister();
  unsigned stackSize = MF->getFrameInfo().getStackSize();

  if (OutStreamer->hasRawTextSupport()) {
	  OutStreamer->EmitRawText("\t.frame\t$" +
			  StringRef(SampleCGInstPrinter::getRegisterName(stackReg)).lower() +
			  "," + Twine(stackSize) + ",$" +
			  StringRef(SampleCGInstPrinter::getRegisterName(returnReg)).lower());
  }
}

/// Emit Set directives.
const char *SampleCGAsmPrinter::getCurrentABIString() const {
  switch (static_cast<SampleCGTargetMachine &>(TM).getABI().GetEnumValue()) {
  case SampleCGABIInfo::ABI::O32:  return "abiO32";
  case SampleCGABIInfo::ABI::S32:  return "abiS32";
  default: llvm_unreachable("Unknown SampleCG ABI");
  }
}

void SampleCGAsmPrinter::EmitFunctionEntryLabel() {
	if (OutStreamer->hasRawTextSupport()) {
		OutStreamer->EmitRawText("\t.ent\t" + Twine(CurrentFnSym->getName()));
	}
	OutStreamer->EmitLabel(CurrentFnSym);
}

/// EmitFunctionBodyStart - Targets can override this to emit stuff before
/// the first basic block in the function.
void SampleCGAsmPrinter::EmitFunctionBodyStart() {
  MCInstLowering.Initialize(&MF->getContext());

  emitFrameDirective();

  if (OutStreamer->hasRawTextSupport()) {
	  SmallString<128> Str;
	  raw_svector_ostream OS(Str);
	  printSavedRegsBitmask(OS);
	  OutStreamer->EmitRawText(OS.str());
	  OutStreamer->EmitRawText(StringRef("\t.set\tnoreorder"));
	  OutStreamer->EmitRawText(StringRef("\t.set\tnomacro"));
	  if (SampleCGFI->getEmitNOAT()) {
		  OutStreamer->EmitRawText(StringRef("\t.set\tnoat"));
	  }
  }

  bool EmitCPLoad = (MF->getTarget().getRelocationModel() == Reloc::PIC_) &&
	  					SampleCGFI->globalBaseRegSet() && SampleCGFI->globalBaseRegFixed();

  if (SampleCGNoCpload)
	  EmitCPLoad = false;

  if (EmitCPLoad) {
	  OutStreamer->EmitRawText(StringRef("\t.cpload\t$t9"));
  }
	else if (EmitCPLoad) {
		SmallVector<MCInst, 4> MCInsts;
		MCInstLowering.LowerCPLOAD(MCInsts);
		for(SmallVector<MCInst, 4>::iterator I = MCInsts.begin(); I != MCInsts.end(); ++I)
			OutStreamer->EmitInstruction(*I, getSubtargetInfo());
	}
}

/// EmitFunctionBodyEnd - Targets can override this to emit stuff after
/// the last basic block in the function.
void SampleCGAsmPrinter::EmitFunctionBodyEnd() {
	if (OutStreamer->hasRawTextSupport()) {
		if (SampleCGFI->getEmitNOAT()) {
			OutStreamer->EmitRawText(StringRef("\t.set\tat"));
		}
		OutStreamer->EmitRawText(StringRef("\t.set\tmacro"));
		OutStreamer->EmitRawText(StringRef("\t.set\treorder"));
		OutStreamer->EmitRawText("\t.end\t" + Twine(CurrentFnSym->getName()));
	}
}

void SampleCGAsmPrinter::EmitStartOfAsmFile(Module &M) {
	if (OutStreamer->hasRawTextSupport()) {
		OutStreamer->EmitRawText("\t.section .mdebug." +
				Twine(getCurrentABIString()));
		OutStreamer->EmitRawText(StringRef("\t.previous"));
	}
}

void SampleCGAsmPrinter::PrintDebugValueComment(const MachineInstr *MI,
                                           raw_ostream &OS) {
  // TODO: implement
  OS << "PrintDebugValueComment()";
}

// Print out an operand for an inline asm expression.
bool SampleCGAsmPrinter::PrintAsmOperand(const MachineInstr *MI, unsigned OpNum,
                                     unsigned AsmVariant,const char *ExtraCode,
                                     raw_ostream &O) {
  // Does this asm operand have a single letter operand modifier?
  if (ExtraCode && ExtraCode[0]) {
    if (ExtraCode[1] != 0) return true; // Unknown modifier.

    const MachineOperand &MO = MI->getOperand(OpNum);
    switch (ExtraCode[0]) {
    default:
      // See if this is a generic print operand
      return AsmPrinter::PrintAsmOperand(MI,OpNum,AsmVariant,ExtraCode,O);
    case 'X': // hex const int
      if ((MO.getType()) != MachineOperand::MO_Immediate)
        return true;
      O << "0x" << StringRef(utohexstr(MO.getImm())).lower();
      return false;
    case 'x': // hex const int (low 16 bits)
      if ((MO.getType()) != MachineOperand::MO_Immediate)
        return true;
      O << "0x" << StringRef(utohexstr(MO.getImm() & 0xffff)).lower();
      return false;
    case 'd': // decimal const int
      if ((MO.getType()) != MachineOperand::MO_Immediate)
        return true;
      O << MO.getImm();
      return false;
    case 'm': // decimal const int minus 1
      if ((MO.getType()) != MachineOperand::MO_Immediate)
        return true;
      O << MO.getImm() - 1;
      return false;
    case 'z': {
      // $0 if zero, regular printing otherwise
      if (MO.getType() != MachineOperand::MO_Immediate)
        return true;
      int64_t Val = MO.getImm();
      if (Val)
        O << Val;
      else
        O << "$0";
      return false;
    }
    }
  }

  printOperand(MI, OpNum, O);
  return false;
}

bool SampleCGAsmPrinter::PrintAsmMemoryOperand(const MachineInstr *MI,
                                           unsigned OpNum, unsigned AsmVariant,
                                           const char *ExtraCode,
                                           raw_ostream &O) {
  int Offset = 0;
  // Currently we are expecting either no ExtraCode or 'D'
  if (ExtraCode) {
    return true; // Unknown modifier.
  }

  const MachineOperand &MO = MI->getOperand(OpNum);
  assert(MO.isReg() && "unexpected inline asm memory operand");
  O << Offset << "($" << SampleCGInstPrinter::getRegisterName(MO.getReg()) << ")";

  return false;
}

void SampleCGAsmPrinter::printOperand(const MachineInstr *MI, int opNum,
                                  raw_ostream &O) {
  const MachineOperand &MO = MI->getOperand(opNum);
  bool closeP = false;

  if (MO.getTargetFlags())
    closeP = true;

  switch(MO.getTargetFlags()) {
  case SampleCGII::MO_GPREL:    O << "%gp_rel("; break;
  case SampleCGII::MO_GOT_CALL: O << "%call16("; break;
  case SampleCGII::MO_GOT:      O << "%got(";    break;
  case SampleCGII::MO_ABS_HI:   O << "%hi(";     break;
  case SampleCGII::MO_ABS_LO:   O << "%lo(";     break;
  case SampleCGII::MO_GOT_HI16: O << "%got_hi16("; break;
  case SampleCGII::MO_GOT_LO16: O << "%got_lo16("; break;
  }

  switch (MO.getType()) {
    case MachineOperand::MO_Register:
      O << '$'
        << StringRef(SampleCGInstPrinter::getRegisterName(MO.getReg())).lower();
      break;

    case MachineOperand::MO_Immediate:
      O << MO.getImm();
      break;

    case MachineOperand::MO_MachineBasicBlock:
      O << *MO.getMBB()->getSymbol();
      return;

    case MachineOperand::MO_GlobalAddress:
      O << *getSymbol(MO.getGlobal());
      break;

    case MachineOperand::MO_BlockAddress: {
      MCSymbol *BA = GetBlockAddressSymbol(MO.getBlockAddress());
      O << BA->getName();
      break;
    }

    case MachineOperand::MO_ExternalSymbol:
      O << *GetExternalSymbolSymbol(MO.getSymbolName());
      break;

    case MachineOperand::MO_JumpTableIndex:
      O << getDataLayout().getPrivateGlobalPrefix() << "JTI" << getFunctionNumber()
        << '_' << MO.getIndex();
      break;

    case MachineOperand::MO_ConstantPoolIndex:
      O << getDataLayout().getPrivateGlobalPrefix() << "CPI"
        << getFunctionNumber() << "_" << MO.getIndex();
      if (MO.getOffset())
        O << "+" << MO.getOffset();
      break;

    default:
      llvm_unreachable("<unknown operand type>");
  }

  if (closeP) O << ")";
}

// Force static initialization.
extern "C" void LLVMInitializeSampleCGAsmPrinter() {
  RegisterAsmPrinter<SampleCGAsmPrinter> X(getTheSampleCGTarget());
  RegisterAsmPrinter<SampleCGAsmPrinter> Y(getTheSampleCGelTarget());
}
