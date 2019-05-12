//===- SampleCGMCInstLower.h - Lower MachineInstr to MCInst --------*- C++ -*--===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_SAMPLECG_SAMPLECGMCINSTLOWER_H
#define LLVM_LIB_TARGET_SAMPLECG_SAMPLECGMCINSTLOWER_H

#include "MCTargetDesc/SampleCGMCExpr.h"
#include "llvm/CodeGen/MachineOperand.h"
#include "llvm/Support/Compiler.h"

namespace llvm {

class MachineBasicBlock;
class MachineInstr;
class MCContext;
class MCInst;
class MCOperand;
class SampleCGAsmPrinter;

/// SampleCGMCInstLower - This class is used to lower an MachineInstr into an
///                   MCInst.
class LLVM_LIBRARY_VISIBILITY SampleCGMCInstLower {
  using MachineOperandType = MachineOperand::MachineOperandType;

  MCContext *Ctx;
  SampleCGAsmPrinter &AsmPrinter;

public:
  SampleCGMCInstLower(SampleCGAsmPrinter &asmprinter);

  void Initialize(MCContext *C);
  void Lower(const MachineInstr *MI, MCInst &OutMI) const;
  MCOperand LowerOperand(const MachineOperand& MO, unsigned offset = 0) const;
  void LowerCPLOAD(SmallVector<MCInst, 4>& MCInsts);
#ifdef ENABLE_GPRESTORE
  void LowerCPRESTORE(int64_t Offset, SmallVector<MCInst, 4>& MCInsts);
#endif

private:
  MCOperand LowerSymbolOperand(const MachineOperand &MO,
		  	MachineOperandType MOTy, unsigned Offset) const;
  MCOperand createSub(MachineBasicBlock*BB1, MachineBasicBlock*BB2,
		  SampleCGMCExpr::SampleCGExprKind Kind) const;
  void lowerLongBranchLUi(const MachineInstr*MI, MCInst &OutMI) const;
  void lowerLongBranchADDiu(const MachineInstr*MI, MCInst &OutMI,
		  int Opcode,SampleCGMCExpr::SampleCGExprKind Kind) const;
  bool lowerLongBranch(const MachineInstr*MI, MCInst &OutMI) const;

};

} // end namespace llvm

#endif // LLVM_LIB_TARGET_SAMPLECG_SAMPLECGMCINSTLOWER_H
