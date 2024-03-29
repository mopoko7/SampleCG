//===-- SampleCGAsmParser.cpp - Parse SampleCG assembly to MCInst instructions ----===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "SampleCG.h"

#include "MCTargetDesc/SampleCGMCExpr.h"
#include "MCTargetDesc/SampleCGMCTargetDesc.h"
#include "SampleCGRegisterInfo.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCInstBuilder.h"
#include "llvm/MC/MCParser/MCAsmLexer.h"
#include "llvm/MC/MCParser/MCParsedAsmOperand.h"
#include "llvm/MC/MCParser/MCTargetAsmParser.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/MC/MCParser/MCAsmLexer.h"
#include "llvm/MC/MCParser/MCParsedAsmOperand.h"
#include "llvm/MC/MCValue.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/MathExtras.h"
#include "llvm/Support/TargetRegistry.h"

using namespace llvm;

#define DEBUG_TYPE "samplecg-asm-parser"

namespace {
class SampleCGAssemblerOptions {
public:
  SampleCGAssemblerOptions():
    reorder(true), macro(true) {
  }

  bool isReorder() {return reorder;}
  void setReorder() {reorder = true;}
  void setNoreorder() {reorder = false;}

  bool isMacro() {return macro;}
  void setMacro() {macro = true;}
  void setNomacro() {macro = false;}

private:
  bool reorder;
  bool macro;
};
}

namespace {
class SampleCGAsmParser : public MCTargetAsmParser {
  MCAsmParser &Parser;
  SampleCGAssemblerOptions Options;

#define GET_ASSEMBLER_HEADER
#include "SampleCGGenAsmMatcher.inc"

  bool MatchAndEmitInstruction(SMLoc IDLoc, unsigned &Opcode,
                               OperandVector &Operands, MCStreamer &Out,
                               uint64_t &ErrorInfo,
                               bool MatchingInlineAsm) override;

  bool ParseRegister(unsigned &RegNo, SMLoc &StartLoc, SMLoc &EndLoc) override;

  bool ParseInstruction(ParseInstructionInfo &Info, StringRef Name,
                        SMLoc NameLoc, OperandVector &Operands) override;

  bool parseMathOperation(StringRef Name, SMLoc NameLoc,
                        OperandVector &Operands);

  bool ParseDirective(AsmToken DirectiveID) override;

  OperandMatchResultTy parseMemOperand(OperandVector &);

  bool ParseOperand(OperandVector &Operands, StringRef Mnemonic);

  int tryParseRegister(StringRef Mnemonic);

  bool tryParseRegisterOperand(OperandVector &Operands,
                               StringRef Mnemonic);

  bool needsExpansion(MCInst &Inst);

  void expandInstruction(MCInst &Inst, SMLoc IDLoc,
                         SmallVectorImpl<MCInst> &Instructions);
  void expandLoadImm(MCInst &Inst, SMLoc IDLoc,
                     SmallVectorImpl<MCInst> &Instructions);
  void expandLoadAddressImm(MCInst &Inst, SMLoc IDLoc,
                            SmallVectorImpl<MCInst> &Instructions);
  void expandLoadAddressReg(MCInst &Inst, SMLoc IDLoc,
                            SmallVectorImpl<MCInst> &Instructions);
  bool reportParseError(StringRef ErrorMsg);

  bool parseMemOffset(const MCExpr *&Res);
  bool parseRelocOperand(const MCExpr *&Res);

  const MCExpr *evaluateRelocExpr(const MCExpr *Expr, StringRef RelocStr);

  bool parseDirectiveSet();

  bool parseSetAtDirective();
  bool parseSetNoAtDirective();
  bool parseSetMacroDirective();
  bool parseSetNoMacroDirective();
  bool parseSetReorderDirective();
  bool parseSetNoReorderDirective();

  int matchRegisterName(StringRef Symbol);

  int matchRegisterByNumber(unsigned RegNum, StringRef Mnemonic);

  unsigned getReg(int RC,int RegNo);

public:
  SampleCGAsmParser(const MCSubtargetInfo &sti, MCAsmParser &parser,
                const MCInstrInfo &MII, const MCTargetOptions &Options)
    : MCTargetAsmParser(Options, sti, MII), Parser(parser) {
    // Initialize the set of available features.
    setAvailableFeatures(ComputeAvailableFeatures(getSTI().getFeatureBits()));
  }

  MCAsmParser &getParser() const { return Parser; }
  MCAsmLexer &getLexer() const { return Parser.getLexer(); }

};
}

namespace {

/// SampleCGOperand - Instances of this class represent a parsed SampleCG machine
/// instruction.
class SampleCGOperand : public MCParsedAsmOperand {

  enum KindTy {
    k_Immediate,
    k_Memory,
    k_Register,
    k_Token
  } Kind;

public:
  SampleCGOperand(KindTy K) : MCParsedAsmOperand(), Kind(K) {}

  struct Token {
    const char *Data;
    unsigned Length;
  };
  struct PhysRegOp {
    unsigned RegNum; /// Register Number
  };
  struct ImmOp {
    const MCExpr *Val;
  };
  struct MemOp {
    unsigned Base;
    const MCExpr *Off;
  };

  union {
    struct Token Tok;
    struct PhysRegOp Reg;
    struct ImmOp Imm;
    struct MemOp Mem;
  };

  SMLoc StartLoc, EndLoc;

public:
  void addRegOperands(MCInst &Inst, unsigned N) const {
    assert(N == 1 && "Invalid number of operands!");
    Inst.addOperand(MCOperand::createReg(getReg()));
  }

  void addExpr(MCInst &Inst, const MCExpr *Expr) const{
    // Add as immediate when possible.  Null MCExpr = 0.
    if (Expr == 0)
      Inst.addOperand(MCOperand::createImm(0));
    else if (const MCConstantExpr *CE = dyn_cast<MCConstantExpr>(Expr))
      Inst.addOperand(MCOperand::createImm(CE->getValue()));
    else
      Inst.addOperand(MCOperand::createExpr(Expr));
  }

  void addImmOperands(MCInst &Inst, unsigned N) const {
    assert(N == 1 && "Invalid number of operands!");
    const MCExpr *Expr = getImm();
    addExpr(Inst,Expr);
  }

  void addMemOperands(MCInst &Inst, unsigned N) const {
    assert(N == 2 && "Invalid number of operands!");

    Inst.addOperand(MCOperand::createReg(getMemBase()));

    const MCExpr *Expr = getMemOff();
    addExpr(Inst,Expr);
  }

  bool isReg() const override { return Kind == k_Register; }
  bool isImm() const override { return Kind == k_Immediate; }
  bool isToken() const override { return Kind == k_Token; }
  bool isMem() const override { return Kind == k_Memory; }

  StringRef getToken() const {
    assert(Kind == k_Token && "Invalid access!");
    return StringRef(Tok.Data, Tok.Length);
  }

  unsigned getReg() const override {
    assert((Kind == k_Register) && "Invalid access!");
    return Reg.RegNum;
  }

  const MCExpr *getImm() const {
    assert((Kind == k_Immediate) && "Invalid access!");
    return Imm.Val;
  }

  unsigned getMemBase() const {
    assert((Kind == k_Memory) && "Invalid access!");
    return Mem.Base;
  }

  const MCExpr *getMemOff() const {
    assert((Kind == k_Memory) && "Invalid access!");
    return Mem.Off;
  }

  static std::unique_ptr<SampleCGOperand> CreateToken(StringRef Str, SMLoc S) {
    auto Op = make_unique<SampleCGOperand>(k_Token);
    Op->Tok.Data = Str.data();
    Op->Tok.Length = Str.size();
    Op->StartLoc = S;
    Op->EndLoc = S;
    return Op;
  }

  /// Internal constructor for register kinds
  static std::unique_ptr<SampleCGOperand> CreateReg(unsigned RegNum, SMLoc S, 
                                                SMLoc E) {
    auto Op = make_unique<SampleCGOperand>(k_Register);
    Op->Reg.RegNum = RegNum;
    Op->StartLoc = S;
    Op->EndLoc = E;
    return Op;
  }

  static std::unique_ptr<SampleCGOperand> CreateImm(const MCExpr *Val, SMLoc S, SMLoc E) {
    auto Op = make_unique<SampleCGOperand>(k_Immediate);
    Op->Imm.Val = Val;
    Op->StartLoc = S;
    Op->EndLoc = E;
    return Op;
  }

  static std::unique_ptr<SampleCGOperand> CreateMem(unsigned Base, const MCExpr *Off,
                                 SMLoc S, SMLoc E) {
    auto Op = make_unique<SampleCGOperand>(k_Memory);
    Op->Mem.Base = Base;
    Op->Mem.Off = Off;
    Op->StartLoc = S;
    Op->EndLoc = E;
    return Op;
  }

  /// getStartLoc - Get the location of the first token of this operand.
  SMLoc getStartLoc() const override { return StartLoc; }
  /// getEndLoc - Get the location of the last token of this operand.
  SMLoc getEndLoc() const override { return EndLoc; }

  void print(raw_ostream &OS) const override {
    switch (Kind) {
    case k_Immediate:
      OS << "Imm<";
      OS << *Imm.Val;
      OS << ">";
      break;
    case k_Memory:
      OS << "Mem<";
      OS << Mem.Base;
      OS << ", ";
      OS << *Mem.Off;
      OS << ">";
      break;
    case k_Register:
      OS << "Register<" << Reg.RegNum << ">";
      break;
    case k_Token:
      OS << Tok.Data;
      break;
    }
  }
};
}

void printSampleCGOperands(OperandVector &Operands) {
  for (size_t i = 0; i < Operands.size(); i++) {
    SampleCGOperand* op = static_cast<SampleCGOperand*>(&*Operands[i]);
    assert(op != nullptr);
    LLVM_DEBUG(dbgs() << "<" << *op << ">");
  }
  LLVM_DEBUG(dbgs() << "\n");
}

//@1 {
bool SampleCGAsmParser::needsExpansion(MCInst &Inst) {

  switch(Inst.getOpcode()) {
    case SampleCG::LoadImm32Reg:
    case SampleCG::LoadAddr32Imm:
    case SampleCG::LoadAddr32Reg:
      return true;
    default:
      return false;
  }
}

void SampleCGAsmParser::expandInstruction(MCInst &Inst, SMLoc IDLoc,
                        SmallVectorImpl<MCInst> &Instructions){
  switch(Inst.getOpcode()) {
    case SampleCG::LoadImm32Reg:
      return expandLoadImm(Inst, IDLoc, Instructions);
    case SampleCG::LoadAddr32Imm:
      return expandLoadAddressImm(Inst,IDLoc,Instructions);
    case SampleCG::LoadAddr32Reg:
      return expandLoadAddressReg(Inst,IDLoc,Instructions);
    }
}
//@1 }

void SampleCGAsmParser::expandLoadImm(MCInst &Inst, SMLoc IDLoc,
                                  SmallVectorImpl<MCInst> &Instructions){
  MCInst tmpInst;
  const MCOperand &ImmOp = Inst.getOperand(1);
  assert(ImmOp.isImm() && "expected immediate operand kind");
  const MCOperand &RegOp = Inst.getOperand(0);
  assert(RegOp.isReg() && "expected register operand kind");

  int ImmValue = ImmOp.getImm();
  tmpInst.setLoc(IDLoc);
  if ( 0 <= ImmValue && ImmValue <= 65535) {
    // for 0 <= j <= 65535.
    // li d,j => ori d,$zero,j
    tmpInst.setOpcode(SampleCG::ORi);
    tmpInst.addOperand(MCOperand::createReg(RegOp.getReg()));
    tmpInst.addOperand(
              MCOperand::createReg(SampleCG::ZERO));
    tmpInst.addOperand(MCOperand::createImm(ImmValue));
    Instructions.push_back(tmpInst);
  } else if ( ImmValue < 0 && ImmValue >= -32768) {
    // for -32768 <= j < 0.
    // li d,j => addiu d,$zero,j
    tmpInst.setOpcode(SampleCG::ADDiu); //TODO:no ADDiu64 in td files?
    tmpInst.addOperand(MCOperand::createReg(RegOp.getReg()));
    tmpInst.addOperand(
              MCOperand::createReg(SampleCG::ZERO));
    tmpInst.addOperand(MCOperand::createImm(ImmValue));
    Instructions.push_back(tmpInst);
  } else {
    // for any other value of j that is representable as a 32-bit integer.
    // li d,j => lui d,hi16(j)
    //           ori d,d,lo16(j)
    tmpInst.setOpcode(SampleCG::LUi);
    tmpInst.addOperand(MCOperand::createReg(RegOp.getReg()));
    tmpInst.addOperand(MCOperand::createImm((ImmValue & 0xffff0000) >> 16));
    Instructions.push_back(tmpInst);
    tmpInst.clear();
    tmpInst.setOpcode(SampleCG::ORi);
    tmpInst.addOperand(MCOperand::createReg(RegOp.getReg()));
    tmpInst.addOperand(MCOperand::createReg(RegOp.getReg()));
    tmpInst.addOperand(MCOperand::createImm(ImmValue & 0xffff));
    tmpInst.setLoc(IDLoc);
    Instructions.push_back(tmpInst);
  }
}

void SampleCGAsmParser::expandLoadAddressReg(MCInst &Inst, SMLoc IDLoc,
                                         SmallVectorImpl<MCInst> &Instructions){
  MCInst tmpInst;
  const MCOperand &ImmOp = Inst.getOperand(2);
  assert(ImmOp.isImm() && "expected immediate operand kind");
  const MCOperand &SrcRegOp = Inst.getOperand(1);
  assert(SrcRegOp.isReg() && "expected register operand kind");
  const MCOperand &DstRegOp = Inst.getOperand(0);
  assert(DstRegOp.isReg() && "expected register operand kind");
  int ImmValue = ImmOp.getImm();
  if ( -32768 <= ImmValue && ImmValue <= 32767) {
    // for -32768 <= j < 32767.
    //la d,j(s) => addiu d,s,j
    tmpInst.setOpcode(SampleCG::ADDiu); //TODO:no ADDiu64 in td files?
    tmpInst.addOperand(MCOperand::createReg(DstRegOp.getReg()));
    tmpInst.addOperand(MCOperand::createReg(SrcRegOp.getReg()));
    tmpInst.addOperand(MCOperand::createImm(ImmValue));
    Instructions.push_back(tmpInst);
  } else {
    // for any other value of j that is representable as a 32-bit integer.
    // la d,j(s) => lui d,hi16(j)
    //              ori d,d,lo16(j)
    //              add d,d,s
    tmpInst.setOpcode(SampleCG::LUi);
    tmpInst.addOperand(MCOperand::createReg(DstRegOp.getReg()));
    tmpInst.addOperand(MCOperand::createImm((ImmValue & 0xffff0000) >> 16));
    Instructions.push_back(tmpInst);
    tmpInst.clear();
    tmpInst.setOpcode(SampleCG::ORi);
    tmpInst.addOperand(MCOperand::createReg(DstRegOp.getReg()));
    tmpInst.addOperand(MCOperand::createReg(DstRegOp.getReg()));
    tmpInst.addOperand(MCOperand::createImm(ImmValue & 0xffff));
    Instructions.push_back(tmpInst);
    tmpInst.clear();
    tmpInst.setOpcode(SampleCG::ADD);
    tmpInst.addOperand(MCOperand::createReg(DstRegOp.getReg()));
    tmpInst.addOperand(MCOperand::createReg(DstRegOp.getReg()));
    tmpInst.addOperand(MCOperand::createReg(SrcRegOp.getReg()));
    Instructions.push_back(tmpInst);
  }
}

void SampleCGAsmParser::expandLoadAddressImm(MCInst &Inst, SMLoc IDLoc,
                                         SmallVectorImpl<MCInst> &Instructions){
  MCInst tmpInst;
  const MCOperand &ImmOp = Inst.getOperand(1);
  assert(ImmOp.isImm() && "expected immediate operand kind");
  const MCOperand &RegOp = Inst.getOperand(0);
  assert(RegOp.isReg() && "expected register operand kind");
  int ImmValue = ImmOp.getImm();
  if ( -32768 <= ImmValue && ImmValue <= 32767) {
    // for -32768 <= j < 32767.
    //la d,j => addiu d,$zero,j
    tmpInst.setOpcode(SampleCG::ADDiu);
    tmpInst.addOperand(MCOperand::createReg(RegOp.getReg()));
    tmpInst.addOperand(
              MCOperand::createReg(SampleCG::ZERO));
    tmpInst.addOperand(MCOperand::createImm(ImmValue));
    Instructions.push_back(tmpInst);
  } else {
    // for any other value of j that is representable as a 32-bit integer.
    // la d,j => lui d,hi16(j)
    //           ori d,d,lo16(j)
    tmpInst.setOpcode(SampleCG::LUi);
    tmpInst.addOperand(MCOperand::createReg(RegOp.getReg()));
    tmpInst.addOperand(MCOperand::createImm((ImmValue & 0xffff0000) >> 16));
    Instructions.push_back(tmpInst);
    tmpInst.clear();
    tmpInst.setOpcode(SampleCG::ORi);
    tmpInst.addOperand(MCOperand::createReg(RegOp.getReg()));
    tmpInst.addOperand(MCOperand::createReg(RegOp.getReg()));
    tmpInst.addOperand(MCOperand::createImm(ImmValue & 0xffff));
    Instructions.push_back(tmpInst);
  }
}

//@2 {
bool SampleCGAsmParser::MatchAndEmitInstruction(SMLoc IDLoc, unsigned &Opcode,
                                            OperandVector &Operands,
                                            MCStreamer &Out,
                                            uint64_t &ErrorInfo,
                                            bool MatchingInlineAsm) {
  printSampleCGOperands(Operands);
  MCInst Inst;
  unsigned MatchResult = MatchInstructionImpl(Operands, Inst, ErrorInfo,
                                              MatchingInlineAsm);
  switch (MatchResult) {
  default: break;
  case Match_Success: {
    if (needsExpansion(Inst)) {
      SmallVector<MCInst, 4> Instructions;
      expandInstruction(Inst, IDLoc, Instructions);
      for(unsigned i =0; i < Instructions.size(); i++){
        Out.EmitInstruction(Instructions[i], getSTI());
      }
    } else {
        Inst.setLoc(IDLoc);
        Out.EmitInstruction(Inst, getSTI());
      }
    return false;
  }
//@2 }
  case Match_MissingFeature:
    Error(IDLoc, "instruction requires a CPU feature not currently enabled");
    return true;
  case Match_InvalidOperand: {
    SMLoc ErrorLoc = IDLoc;
    if (ErrorInfo != ~0U) {
      if (ErrorInfo >= Operands.size())
        return Error(IDLoc, "too few operands for instruction");

      ErrorLoc = ((SampleCGOperand &)*Operands[ErrorInfo]).getStartLoc();
      if (ErrorLoc == SMLoc()) ErrorLoc = IDLoc;
    }

    return Error(ErrorLoc, "invalid operand for instruction");
  }
  case Match_MnemonicFail:
    return Error(IDLoc, "invalid instruction");
  }
  return true;
}

int SampleCGAsmParser::matchRegisterName(StringRef Name) {

   int CC;
    CC = StringSwitch<unsigned>(Name)
      .Case("zero",  SampleCG::ZERO)
      .Case("at",  SampleCG::AT)
      .Case("v0",  SampleCG::V0)
      .Case("v1",  SampleCG::V1)
      .Case("a0",  SampleCG::A0)
      .Case("a1",  SampleCG::A1)
      .Case("t9",  SampleCG::T9)
      .Case("t0",  SampleCG::T0)
      .Case("t1",  SampleCG::T1)
      .Case("s0",  SampleCG::S0)
      .Case("s1",  SampleCG::S1)
      .Case("sw",  SampleCG::SW)
      .Case("gp",  SampleCG::GP)
      .Case("fp",  SampleCG::FP)
      .Case("sp",  SampleCG::SP)
      .Case("lr",  SampleCG::LR)
      .Case("pc",  SampleCG::PC)
      .Case("hi",  SampleCG::HI)
      .Case("lo",  SampleCG::LO)
      .Case("epc", SampleCG::EPC)
      .Default(-1);

  if (CC != -1)
    return CC;

  return -1;
}

unsigned SampleCGAsmParser::getReg(int RC,int RegNo) {
  return *(getContext().getRegisterInfo()->getRegClass(RC).begin() + RegNo);
}

int SampleCGAsmParser::matchRegisterByNumber(unsigned RegNum, StringRef Mnemonic) {
  if (RegNum > 15)
    return -1;

  return getReg(SampleCG::CPURegsRegClassID, RegNum);
}

int SampleCGAsmParser::tryParseRegister(StringRef Mnemonic) {
  const AsmToken &Tok = Parser.getTok();
  int RegNum = -1;

  if (Tok.is(AsmToken::Identifier)) {
    std::string lowerCase = Tok.getString().lower();
    RegNum = matchRegisterName(lowerCase);
  } else if (Tok.is(AsmToken::Integer))
    RegNum = matchRegisterByNumber(static_cast<unsigned>(Tok.getIntVal()),
                                   Mnemonic.lower());
    else
      return RegNum;  //error
  return RegNum;
}

bool SampleCGAsmParser::
  tryParseRegisterOperand(OperandVector &Operands,
                          StringRef Mnemonic){

  SMLoc S = Parser.getTok().getLoc();
  int RegNo = -1;

    RegNo = tryParseRegister(Mnemonic);
  if (RegNo == -1)
    return true;

  Operands.push_back(SampleCGOperand::CreateReg(RegNo, S,
      Parser.getTok().getLoc()));
  Parser.Lex(); // Eat register token.
  return false;
}

bool SampleCGAsmParser::ParseOperand(OperandVector &Operands,
                                 StringRef Mnemonic) {
  LLVM_DEBUG(dbgs() << "ParseOperand\n");
  // Check if the current operand has a custom associated parser, if so, try to
  // custom parse the operand, or fallback to the general approach.
  OperandMatchResultTy ResTy = MatchOperandParserImpl(Operands, Mnemonic);
  if (ResTy == MatchOperand_Success)
    return false;
  // If there wasn't a custom match, try the generic matcher below. Otherwise,
  // there was a match, but an error occurred, in which case, just return that
  // the operand parsing failed.
  if (ResTy == MatchOperand_ParseFail)
    return true;

  LLVM_DEBUG(dbgs() << ".. Generic Parser\n");

  switch (getLexer().getKind()) {
  default:
    Error(Parser.getTok().getLoc(), "unexpected token in operand");
    return true;
  case AsmToken::Dollar: {
    // parse register
    SMLoc S = Parser.getTok().getLoc();
    Parser.Lex(); // Eat dollar token.
    // parse register operand
    if (!tryParseRegisterOperand(Operands, Mnemonic)) {
      if (getLexer().is(AsmToken::LParen)) {
        // check if it is indexed addressing operand
        Operands.push_back(SampleCGOperand::CreateToken("(", S));
        Parser.Lex(); // eat parenthesis
        if (getLexer().isNot(AsmToken::Dollar))
          return true;

        Parser.Lex(); // eat dollar
        if (tryParseRegisterOperand(Operands, Mnemonic))
          return true;

        if (!getLexer().is(AsmToken::RParen))
          return true;

        S = Parser.getTok().getLoc();
        Operands.push_back(SampleCGOperand::CreateToken(")", S));
        Parser.Lex();
      }
      return false;
    }
    // maybe it is a symbol reference
    StringRef Identifier;
    if (Parser.parseIdentifier(Identifier))
      return true;

    SMLoc E = SMLoc::getFromPointer(Parser.getTok().getLoc().getPointer() - 1);

    MCSymbol *Sym = getContext().getOrCreateSymbol("$" + Identifier);

    // Otherwise create a symbol ref.
    const MCExpr *Res = MCSymbolRefExpr::create(Sym, MCSymbolRefExpr::VK_None,
                                                getContext());

    Operands.push_back(SampleCGOperand::CreateImm(Res, S, E));
    return false;
  }
  case AsmToken::Identifier:
  case AsmToken::LParen:
  case AsmToken::Minus:
  case AsmToken::Plus:
  case AsmToken::Integer:
  case AsmToken::String: {
     // quoted label names
    const MCExpr *IdVal;
    SMLoc S = Parser.getTok().getLoc();
    if (getParser().parseExpression(IdVal))
      return true;
    SMLoc E = SMLoc::getFromPointer(Parser.getTok().getLoc().getPointer() - 1);
    Operands.push_back(SampleCGOperand::CreateImm(IdVal, S, E));
    return false;
  }
  case AsmToken::Percent: {
    // it is a symbol reference or constant expression
    const MCExpr *IdVal;
    SMLoc S = Parser.getTok().getLoc(); // start location of the operand
    if (parseRelocOperand(IdVal))
      return true;

    SMLoc E = SMLoc::getFromPointer(Parser.getTok().getLoc().getPointer() - 1);

    Operands.push_back(SampleCGOperand::CreateImm(IdVal, S, E));
    return false;
  } // case AsmToken::Percent
  } // switch(getLexer().getKind())
  return true;
}

const MCExpr *SampleCGAsmParser::evaluateRelocExpr(const MCExpr *Expr,
                                               StringRef RelocStr) {
  SampleCGMCExpr::SampleCGExprKind Kind =
      StringSwitch<SampleCGMCExpr::SampleCGExprKind>(RelocStr)
          .Case("call16", SampleCGMCExpr::CEK_GOT_CALL)
          .Case("call_hi", SampleCGMCExpr::CEK_CALL_HI16)
          .Case("call_lo", SampleCGMCExpr::CEK_CALL_LO16)
          .Case("dtp_hi", SampleCGMCExpr::CEK_DTP_HI)
          .Case("dtp_lo", SampleCGMCExpr::CEK_DTP_LO)
          .Case("got", SampleCGMCExpr::CEK_GOT)
          .Case("got_hi", SampleCGMCExpr::CEK_GOT_HI16)
          .Case("got_lo", SampleCGMCExpr::CEK_GOT_LO16)
          .Case("gottprel", SampleCGMCExpr::CEK_GOTTPREL)
          .Case("gp_rel", SampleCGMCExpr::CEK_GPREL)
          .Case("hi", SampleCGMCExpr::CEK_ABS_HI)
          .Case("lo", SampleCGMCExpr::CEK_ABS_LO)
          .Case("tlsgd", SampleCGMCExpr::CEK_TLSGD)
          .Case("tlsldm", SampleCGMCExpr::CEK_TLSLDM)
          .Case("tp_hi", SampleCGMCExpr::CEK_TP_HI)
          .Case("tp_lo", SampleCGMCExpr::CEK_TP_LO)
          .Default(SampleCGMCExpr::CEK_None);

  assert(Kind != SampleCGMCExpr::CEK_None);
  return SampleCGMCExpr::create(Kind, Expr, getContext());
}

bool SampleCGAsmParser::parseRelocOperand(const MCExpr *&Res) {

  Parser.Lex(); // eat % token
  const AsmToken &Tok = Parser.getTok(); // get next token, operation
  if (Tok.isNot(AsmToken::Identifier))
    return true;

  std::string Str = Tok.getIdentifier().str();

  Parser.Lex(); // eat identifier
  // now make expression from the rest of the operand
  const MCExpr *IdVal;
  SMLoc EndLoc;

  if (getLexer().getKind() == AsmToken::LParen) {
    while (1) {
      Parser.Lex(); // eat '(' token
      if (getLexer().getKind() == AsmToken::Percent) {
        Parser.Lex(); // eat % token
        const AsmToken &nextTok = Parser.getTok();
        if (nextTok.isNot(AsmToken::Identifier))
          return true;
        Str += "(%";
        Str += nextTok.getIdentifier();
        Parser.Lex(); // eat identifier
        if (getLexer().getKind() != AsmToken::LParen)
          return true;
      } else
        break;
    }
    if (getParser().parseParenExpression(IdVal,EndLoc))
      return true;

    while (getLexer().getKind() == AsmToken::RParen)
      Parser.Lex(); // eat ')' token

  } else
    return true; // parenthesis must follow reloc operand

  Res = evaluateRelocExpr(IdVal, Str);
  return false;
}

bool SampleCGAsmParser::ParseRegister(unsigned &RegNo, SMLoc &StartLoc,
                                  SMLoc &EndLoc) {

  StartLoc = Parser.getTok().getLoc();
  RegNo = tryParseRegister("");
  EndLoc = Parser.getTok().getLoc();
  return (RegNo == (unsigned)-1);
}

bool SampleCGAsmParser::parseMemOffset(const MCExpr *&Res) {

  SMLoc S;

  switch(getLexer().getKind()) {
  default:
    return true;
  case AsmToken::Integer:
  case AsmToken::Minus:
  case AsmToken::Plus:
    return (getParser().parseExpression(Res));
  case AsmToken::Percent:
    return parseRelocOperand(Res);
  case AsmToken::LParen:
    return false;  // it's probably assuming 0
  }
  return true;
}

// eg, 12($sp) or 12(la)
OperandMatchResultTy SampleCGAsmParser::parseMemOperand(
               OperandVector &Operands) {

  const MCExpr *IdVal = 0;
  SMLoc S;
  // first operand is the offset
  S = Parser.getTok().getLoc();

  if (parseMemOffset(IdVal))
    return MatchOperand_ParseFail;

  const AsmToken &Tok = Parser.getTok(); // get next token
  if (Tok.isNot(AsmToken::LParen)) {
    SampleCGOperand &Mnemonic = static_cast<SampleCGOperand &>(*Operands[0]);
    if (Mnemonic.getToken() == "la") {
      SMLoc E = SMLoc::getFromPointer(Parser.getTok().getLoc().getPointer()-1);
      Operands.push_back(SampleCGOperand::CreateImm(IdVal, S, E));
      return MatchOperand_Success;
    }
    Error(Parser.getTok().getLoc(), "'(' expected");
    return MatchOperand_ParseFail;
  }

  Parser.Lex(); // Eat '(' token.

  const AsmToken &Tok1 = Parser.getTok(); // get next token
  if (Tok1.is(AsmToken::Dollar)) {
    Parser.Lex(); // Eat '$' token.
    if (tryParseRegisterOperand(Operands,"")) {
      Error(Parser.getTok().getLoc(), "unexpected token in operand");
      return MatchOperand_ParseFail;
    }

  } else {
    Error(Parser.getTok().getLoc(), "unexpected token in operand");
    return MatchOperand_ParseFail;
  }

  const AsmToken &Tok2 = Parser.getTok(); // get next token
  if (Tok2.isNot(AsmToken::RParen)) {
    Error(Parser.getTok().getLoc(), "')' expected");
    return MatchOperand_ParseFail;
  }

  SMLoc E = SMLoc::getFromPointer(Parser.getTok().getLoc().getPointer() - 1);

  Parser.Lex(); // Eat ')' token.

  if (!IdVal)
    IdVal = MCConstantExpr::create(0, getContext());

  // Replace the register operand with the memory operand.
  std::unique_ptr<SampleCGOperand> op(
      static_cast<SampleCGOperand *>(Operands.back().release()));
  int RegNo = op->getReg();
  // remove register from operands
  Operands.pop_back();
  // and add memory operand
  Operands.push_back(SampleCGOperand::CreateMem(RegNo, IdVal, S, E));
  return MatchOperand_Success;
}

#if 0
bool SampleCGAsmParser::
parseMathOperation(StringRef Name, SMLoc NameLoc,
                   OperandVector &Operands) {
  // split the format
  size_t Start = Name.find('.'), Next = Name.rfind('.');
  StringRef Format1 = Name.slice(Start, Next);
  // and add the first format to the operands
  Operands.push_back(SampleCGOperand::CreateToken(Format1, NameLoc));
  // now for the second format
  StringRef Format2 = Name.slice(Next, StringRef::npos);
  Operands.push_back(SampleCGOperand::CreateToken(Format2, NameLoc));

  // set the format for the first register
//  setFpFormat(Format1);

  // Read the remaining operands.
  if (getLexer().isNot(AsmToken::EndOfStatement)) {
    // Read the first operand.
    if (ParseOperand(Operands, Name)) {
      SMLoc Loc = getLexer().getLoc();
      Parser.eatToEndOfStatement();
      return Error(Loc, "unexpected token in argument list");
    }

    if (getLexer().isNot(AsmToken::Comma)) {
      SMLoc Loc = getLexer().getLoc();
      Parser.eatToEndOfStatement();
      return Error(Loc, "unexpected token in argument list");

    }
    Parser.Lex();  // Eat the comma.

    // Parse and remember the operand.
    if (ParseOperand(Operands, Name)) {
      SMLoc Loc = getLexer().getLoc();
      Parser.eatToEndOfStatement();
      return Error(Loc, "unexpected token in argument list");
    }
  }

  if (getLexer().isNot(AsmToken::EndOfStatement)) {
    SMLoc Loc = getLexer().getLoc();
    Parser.eatToEndOfStatement();
    return Error(Loc, "unexpected token in argument list");
  }

  Parser.Lex(); // Consume the EndOfStatement
  return false;
}
#endif

bool SampleCGAsmParser::
ParseInstruction(ParseInstructionInfo &Info, StringRef Name, SMLoc NameLoc,
                 OperandVector &Operands) {

  // Create the leading tokens for the mnemonic, split by '.' characters.
  size_t Start = 0, Next = Name.find('.');
  StringRef Mnemonic = Name.slice(Start, Next);
  // Refer to the explanation in source code of function DecodeJumpFR(...) in 
  // SampleCGDisassembler.cpp
  if (Mnemonic == "ret")
    Mnemonic = "jr";

  Operands.push_back(SampleCGOperand::CreateToken(Mnemonic, NameLoc));

  // Read the remaining operands.
  if (getLexer().isNot(AsmToken::EndOfStatement)) {
    // Read the first operand.
    if (ParseOperand(Operands, Name)) {
      SMLoc Loc = getLexer().getLoc();
      Parser.eatToEndOfStatement();
      return Error(Loc, "unexpected token in argument list");
    }

    while (getLexer().is(AsmToken::Comma) ) {
      Parser.Lex();  // Eat the comma.

      // Parse and remember the operand.
      if (ParseOperand(Operands, Name)) {
        SMLoc Loc = getLexer().getLoc();
        Parser.eatToEndOfStatement();
        return Error(Loc, "unexpected token in argument list");
      }
    }
  }

  if (getLexer().isNot(AsmToken::EndOfStatement)) {
    SMLoc Loc = getLexer().getLoc();
    Parser.eatToEndOfStatement();
    return Error(Loc, "unexpected token in argument list");
  }

  Parser.Lex(); // Consume the EndOfStatement
  return false;
}

bool SampleCGAsmParser::reportParseError(StringRef ErrorMsg) {
   SMLoc Loc = getLexer().getLoc();
   Parser.eatToEndOfStatement();
   return Error(Loc, ErrorMsg);
}

bool SampleCGAsmParser::parseSetReorderDirective() {
  Parser.Lex();
  // if this is not the end of the statement, report error
  if (getLexer().isNot(AsmToken::EndOfStatement)) {
    reportParseError("unexpected token in statement");
    return false;
  }
  Options.setReorder();
  Parser.Lex(); // Consume the EndOfStatement
  return false;
}

bool SampleCGAsmParser::parseSetNoReorderDirective() {
    Parser.Lex();
    // if this is not the end of the statement, report error
    if (getLexer().isNot(AsmToken::EndOfStatement)) {
      reportParseError("unexpected token in statement");
      return false;
    }
    Options.setNoreorder();
    Parser.Lex(); // Consume the EndOfStatement
    return false;
}

bool SampleCGAsmParser::parseSetMacroDirective() {
  Parser.Lex();
  // if this is not the end of the statement, report error
  if (getLexer().isNot(AsmToken::EndOfStatement)) {
    reportParseError("unexpected token in statement");
    return false;
  }
  Options.setMacro();
  Parser.Lex(); // Consume the EndOfStatement
  return false;
}

bool SampleCGAsmParser::parseSetNoMacroDirective() {
  Parser.Lex();
  // if this is not the end of the statement, report error
  if (getLexer().isNot(AsmToken::EndOfStatement)) {
    reportParseError("`noreorder' must be set before `nomacro'");
    return false;
  }
  if (Options.isReorder()) {
    reportParseError("`noreorder' must be set before `nomacro'");
    return false;
  }
  Options.setNomacro();
  Parser.Lex(); // Consume the EndOfStatement
  return false;
}
bool SampleCGAsmParser::parseDirectiveSet() {

  // get next token
  const AsmToken &Tok = Parser.getTok();

  if (Tok.getString() == "reorder") {
    return parseSetReorderDirective();
  } else if (Tok.getString() == "noreorder") {
    return parseSetNoReorderDirective();
  } else if (Tok.getString() == "macro") {
    return parseSetMacroDirective();
  } else if (Tok.getString() == "nomacro") {
    return parseSetNoMacroDirective();
  }
  return true;
}

bool SampleCGAsmParser::ParseDirective(AsmToken DirectiveID) {

  if (DirectiveID.getString() == ".ent") {
    // ignore this directive for now
    Parser.Lex();
    return false;
  }

  if (DirectiveID.getString() == ".end") {
    // ignore this directive for now
    Parser.Lex();
    return false;
  }

  if (DirectiveID.getString() == ".frame") {
    // ignore this directive for now
    Parser.eatToEndOfStatement();
    return false;
  }

  if (DirectiveID.getString() == ".set") {
    return parseDirectiveSet();
  }

  if (DirectiveID.getString() == ".fmask") {
    // ignore this directive for now
    Parser.eatToEndOfStatement();
    return false;
  }

  if (DirectiveID.getString() == ".mask") {
    // ignore this directive for now
    Parser.eatToEndOfStatement();
    return false;
  }

  if (DirectiveID.getString() == ".gpword") {
    // ignore this directive for now
    Parser.eatToEndOfStatement();
    return false;
  }

  return true;
}

extern "C" void LLVMInitializeSampleCGAsmParser() {
  RegisterMCAsmParser<SampleCGAsmParser> X(getTheSampleCGTarget());
  RegisterMCAsmParser<SampleCGAsmParser> Y(getTheSampleCGelTarget());
}

#define GET_REGISTER_MATCHER
#define GET_MATCHER_IMPLEMENTATION
#include "SampleCGGenAsmMatcher.inc"

