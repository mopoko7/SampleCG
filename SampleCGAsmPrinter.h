//===- SampleCGAsmPrinter.h - SampleCG LLVM Assembly Printer -----------*- C++ -*--===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// SampleCG Assembly printer class.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_SAMPLECG_SAMPLECGASMPRINTER_H
#define LLVM_LIB_TARGET_SAMPLECG_SAMPLECGASMPRINTER_H

#include "SampleCGMCInstLower.h"
#include "SampleCGSubtarget.h"
#include "llvm/CodeGen/AsmPrinter.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/Support/Compiler.h"
#include <algorithm>
#include <map>
#include <memory>

namespace llvm {

class MCOperand;
class MCSubtargetInfo;
class MCSymbol;
class MachineBasicBlock;
class MachineConstantPool;
class MachineFunction;
class MachineInstr;
class MachineOperand;
class SampleCGFunctionInfo;
class SampleCGTargetStreamer;
class Module;
class raw_ostream;
class TargetMachine;

class LLVM_LIBRARY_VISIBILITY SampleCGAsmPrinter : public AsmPrinter {
  SampleCGTargetStreamer &getTargetStreamer() const;

  void EmitInstrWithMacroNoAT(const MachineInstr *MI);

private:
  // lowerOperand - Convert a MachineOperand into the equivalent MCOperand.
  bool lowerOperand(const MachineOperand &MO, MCOperand &MCOp);

  // tblgen'erated function.
  bool emitPseudoExpansionLowering(MCStreamer &OutStreamer, const MachineInstr *MI);

  bool isLongBranchPseudo(int Opcode) const;

public:
  const SampleCGSubtarget *Subtarget;
  const SampleCGFunctionInfo *SampleCGFI;
  SampleCGMCInstLower MCInstLowering;

  explicit SampleCGAsmPrinter(TargetMachine &TM,
                          std::unique_ptr<MCStreamer> Streamer)
      : AsmPrinter(TM, std::move(Streamer)), MCInstLowering(*this) {}

  StringRef getPassName() const override { return "SampleCG Assembly Printer"; }

  bool runOnMachineFunction(MachineFunction &MF) override;

  void EmitInstruction(const MachineInstr *MI) override;
  void printSavedRegsBitmask(raw_ostream &O);
  void printHex32(unsigned int Value, raw_ostream &O);
  void emitFrameDirective();
  const char *getCurrentABIString() const;
  void EmitFunctionEntryLabel() override;
  void EmitFunctionBodyStart() override;
  void EmitFunctionBodyEnd() override;
  bool PrintAsmOperand(const MachineInstr *MI, unsigned OpNo,
                       const char *ExtraCode,
                       raw_ostream &O) override;
  bool PrintAsmMemoryOperand(const MachineInstr *MI, unsigned OpNum,
                             const char *ExtraCode,
                             raw_ostream &O) override;
  void printOperand(const MachineInstr *MI, int opNum, raw_ostream &O);
  void EmitStartOfAsmFile(Module &M) override;
  void PrintDebugValueComment(const MachineInstr *MI, raw_ostream &OS);
};

} // end namespace llvm

#endif // LLVM_LIB_TARGET_SAMPLECG_SAMPLECGASMPRINTER_H
