//===- SampleCGInstrInfo.td - Target Description for SampleCG Target -*- tablegen -*-=//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the SampleCG implementation of the TargetInstrInfo class.
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Instruction format superclass
//===----------------------------------------------------------------------===//

include "SampleCGInstrFormats.td"

//===----------------------------------------------------------------------===//
// SampleCG profiles and nodes
//===----------------------------------------------------------------------===//

def SDT_SampleCGRet          : SDTypeProfile<0, 1, [SDTCisInt<0>]>;

def SDT_SampleCGJmpLink      : SDTypeProfile<0, 1, [SDTCisVT<0, iPTR>]>;

def SDT_SampleCGCallSeqStart : SDCallSeqStart<[SDTCisVT<0, i32>]>;

def SDT_SampleCGCallSeqEnd   : SDCallSeqEnd<[SDTCisVT<0, i32>, SDTCisVT<1, i32>]>;

// These are target-independent nodes, but have target-specific formats.
def callseq_start : SDNode<"ISD::CALLSEQ_START", SDT_SampleCGCallSeqStart,
							[SDNPHasChain, SDNPSideEffect, SDNPOutGlue]>;

def callseq_end   : SDNode<"ISD::CALLSEQ_END", SDT_SampleCGCallSeqEnd,
                           [SDNPHasChain, SDNPOptInGlue, SDNPOutGlue]>;

def uimm16 : Operand<i32> {
	let PrintMethod = "printUImm<16>";
}

let Defs = [SP], Uses = [SP] in {
def ADJCALLSTACKDOWN : SampleCGPseudo<(outs), (ins uimm16:$amt, uimm16:$amt2),
                                  "!ADJCALLSTACKDOWN $amt",
                                  [(callseq_start timm:$amt, timm:$amt2)]>;
def ADJCALLSTACKUP   : SampleCGPseudo<(outs), (ins uimm16:$amt1, uimm16:$amt2),
                                  "!ADJCALLSTACKUP $amt1",
                                  [(callseq_end timm:$amt1, timm:$amt2)]>;
}

//@def CPRESTORE {
// When handling PIC code the assembler needs .cpload and .cprestore
// directives. If the real instructions corresponding these directives
// are used, we have the same behavior, but get also a bunch of warnings
// from the assembler.
let hasSideEffects = 0 in
def CPRESTORE : SampleCGPseudo<(outs), (ins i32imm:$loc, CPURegs:$gp),
                           ".cprestore\t$loc", []>;

// Call
def SampleCGJmpLink : SDNode<"SampleCGISD::JmpLink", SDT_SampleCGJmpLink,
					[SDNPHasChain, SDNPOutGlue, SDNPOptInGlue,SDNPVariadic]>;

class IsTailCall {
	bit isCall = 1;
	bit isTerminator = 1;
	bit isReturn = 1;
	bit isBarrier = 1;
	bit hasExtraSrcRegAllocReq = 1;
	bit isCodeGenOnly = 1;
}

def calltarget : Operand<iPTR> {
	let EncoderMethod = "getJumpTargetOpValue";
}

// Jump and Link (Call)
let isCall=1, hasDelaySlot=1 in {
  //@JumpLink {
  class JumpLink<bits<8> op, string instr_asm>:
    FJ<op, (outs), (ins calltarget:$target, variable_ops),
       !strconcat(instr_asm, "\t$target"), [(SampleCGJmpLink imm:$target)],
       IIBranch> {
		   let DecoderMethod = "DecodeJumpTarget";
       }
  //@JumpLink }

  class JumpLinkReg<bits<8> op, string instr_asm,
                    RegisterClass RC>:
    FA<op, (outs), (ins RC:$rb, variable_ops),
       !strconcat(instr_asm, "\t$rb"), [(SampleCGJmpLink RC:$rb)], IIBranch> {
    let rc = 0;
    let ra = 14;
    let shamt = 0;
  }
}

def JSUB : JumpLink<0x3b, "jsub">;
def JALR : JumpLinkReg<0x39, "jalr", GPROut>;

def : Pat<(SampleCGJmpLink (i32 tglobaladdr:$dst)),
			(JSUB tglobaladdr:$dst)>;
def : Pat<(SampleCGJmpLink (i32 texternalsym:$dst)),
			(JSUB texternalsym:$dst)>;

// Return
def SampleCGRet : SDNode<"SampleCGISD::Ret", SDTNone,
                     [SDNPHasChain, SDNPOptInGlue, SDNPVariadic]>;

//===----------------------------------------------------------------------===//
// SampleCG Operand, Complex Patterns and Transformations Definitions.
//===----------------------------------------------------------------------===//
// Instruction operand types

// Signed Operand
def simm16      : Operand<i32> {
  let DecoderMethod= "DecodeSimm16";
}

def SDT_SampleCGDivRem : SDTypeProfile<0, 2,
	[SDTCisInt<0>, SDTCisSameAs<0, 1>]>;

def SampleCGDivRem : SDNode<"SampleCGISD::DivRem", SDT_SampleCGDivRem,
	[SDNPOutGlue]>;
	
def SampleCGDivRemU : SDNode<"SampleCGISD::DivRemU", SDT_SampleCGDivRem,
	[SDNPOutGlue]>;

def shamt : Operand<i32>;

def LO16 : SDNodeXForm<imm, [{
	return getImm(N, N->getZExtValue() & 0xffff);
}]>;

def HI16 : SDNodeXForm<imm, [{
	return getImm(N, (N->getZExtValue() >> 16) & 0xffff);
}]>;

def immZExt16 : PatLeaf<(imm), [{
	if (N->getValueType(0) == MVT::i32) {
		return (uint32_t)N->getZExtValue() == (unsigned short)N->getZExtValue();
	}
	else {
		return (uint64_t)N->getZExtValue() == (unsigned short)N->getZExtValue();
	}
}], LO16>;

def immLow16Zero : PatLeaf<(imm), [{
	int64_t Val = N->getSExtValue();
	return isInt<32>(Val) && !(Val & 0xffff);
}]>;

def immZExt5 : ImmLeaf<i32, [{return Imm == (Imm & 0x1f);}]>;

// Node immediate fits as 16-bit sign extended on target immediate.
// e.g. addi, andi
def immSExt16  : PatLeaf<(imm), [{ return isInt<16>(N->getSExtValue()); }]>;

// SampleCG Address Mode! SDNode frameindex could possibily be a match
// since load and store instructions from stack used it.
def addr : 
  ComplexPattern<iPTR, 2, "SelectAddr", [frameindex], [SDNPWantParent]>;

//===----------------------------------------------------------------------===//
// Pattern fragment for load/store
//===----------------------------------------------------------------------===//

class AlignedLoad<PatFrag Node> :
  PatFrag<(ops node:$ptr), (Node node:$ptr), [{
  LoadSDNode *LD = cast<LoadSDNode>(N);
  return LD->getMemoryVT().getSizeInBits()/8 <= LD->getAlignment();
}]>;

class AlignedStore<PatFrag Node> :
  PatFrag<(ops node:$val, node:$ptr), (Node node:$val, node:$ptr), [{
  StoreSDNode *SD = cast<StoreSDNode>(N);
  return SD->getMemoryVT().getSizeInBits()/8 <= SD->getAlignment();
}]>;

// Load/Store PatFrags.
def sextloadi16_a   : AlignedLoad<sextloadi16>;
def zextloadi16_a   : AlignedLoad<zextloadi16>;
def extloadi16_a    : AlignedLoad<extloadi16>;
def load_a          : AlignedLoad<load>;
def truncstorei16_a : AlignedStore<truncstorei16>;
def store_a         : AlignedStore<store>;


//===----------------------------------------------------------------------===//
// Instructions specific format
//===----------------------------------------------------------------------===//

// Arithmetic and logical instructions with 2 register operands.
class ArithLogicI<bits<8> op, string instr_asm, SDNode OpNode,
                  Operand Od, PatLeaf imm_type, RegisterClass RC> :
  FL<op, (outs GPROut:$ra), (ins RC:$rb, Od:$imm16),
     !strconcat(instr_asm, "\t$ra, $rb, $imm16"),
     [(set GPROut:$ra, (OpNode RC:$rb, imm_type:$imm16))], IIAlu> {
  let isReMaterializable = 1;
}

class ArithLogicR<bits<8> op, string instr_asm, SDNode OpNode,
					InstrItinClass itin, RegisterClass RC, bit isComm = 0>:
	FA<op, (outs GPROut:$ra), (ins RC:$rb, RC:$rc),
		!strconcat(instr_asm, "\t$ra, $rb, $rc"),
		[(set GPROut:$ra, (OpNode RC:$rb, RC:$rc))], itin> {
	let shamt = 0;
	let isCommutable = isComm;
	let isReMaterializable = 1;
}

class LoadUpper<bits<8> op, string instr_asm, RegisterClass RC, Operand Imm>:
	FL<op, (outs RC:$ra), (ins Imm:$imm16), 
		!strconcat(instr_asm, "\t$ra, $imm16"), [], IIAlu> {
	let rb = 0;
	let isReMaterializable = 1;
}

class FMem<bits<8> op, dag outs, dag ins, string asmstr, list<dag> pattern,
          InstrItinClass itin>: FL<op, outs, ins, asmstr, pattern, itin> {
  bits<20> addr;
  let Inst{19-16} = addr{19-16};
  let Inst{15-0}  = addr{15-0};
  let DecoderMethod = "DecodeMem";
}

// Memory Load/Store
let canFoldAsLoad = 1 in
class LoadM<bits<8> op, string instr_asm, PatFrag OpNode, RegisterClass RC,
            Operand MemOpnd, bit Pseudo>:
  FMem<op, (outs RC:$ra), (ins MemOpnd:$addr),
     !strconcat(instr_asm, "\t$ra, $addr"),
     [(set RC:$ra, (OpNode addr:$addr))], IILoad> {
  let isPseudo = Pseudo;
}

class StoreM<bits<8> op, string instr_asm, PatFrag OpNode, RegisterClass RC,
             Operand MemOpnd, bit Pseudo>:
  FMem<op, (outs), (ins RC:$ra, MemOpnd:$addr),
     !strconcat(instr_asm, "\t$ra, $addr"),
     [(OpNode RC:$ra, addr:$addr)], IIStore> {
  let isPseudo = Pseudo;
}

def SampleCGMemAsmOperand : AsmOperandClass {
  let Name = "Mem";
  let ParserMethod = "parseMemOperand";
}

// Address operand
def mem : Operand<iPTR> {
  let PrintMethod = "printMemOperand";
  let MIOperandInfo = (ops CPURegs, simm16);
  let EncoderMethod = "getMemEncoding";
  let ParserMatchClass = SampleCGMemAsmOperand;
}


//@ 32-bit load.
multiclass LoadM32<bits<8> op, string instr_asm, PatFrag OpNode,
                   bit Pseudo = 0> {
  def #NAME# : LoadM<op, instr_asm, OpNode, GPROut, mem, Pseudo>;
}

// 32-bit store.
multiclass StoreM32<bits<8> op, string instr_asm, PatFrag OpNode,
                    bit Pseudo = 0> {
  def #NAME# : StoreM<op, instr_asm, OpNode, CPURegs, mem, Pseudo>;
}

//@JumpFR {
let isBranch=1, isTerminator=1, isBarrier=1, imm16=0, hasDelaySlot = 1,
    isIndirectBranch = 1 in
class JumpFR<bits<8> op, string instr_asm, RegisterClass RC>:
  FL<op, (outs), (ins RC:$ra),
     !strconcat(instr_asm, "\t$ra"), [(brind RC:$ra)], IIBranch> {
  let rb = 0;
  let imm16 = 0;
  let DecoderMethod = "DecodeJumpFR";
}
//@JumpFR }

// Return instruction
class RetBase<RegisterClass RC>: JumpFR<0x3c, "ret", RC> {
  let isReturn = 1;
  let isCodeGenOnly = 1;
  let hasCtrlDep = 1;
  let hasExtraSrcRegAllocReq = 1;
}

class shift_rotate_imm<bits<8> op, bits<4> isRotate, string instr_asm,
						SDNode OpNode, PatFrag PF, Operand ImmOpnd,
						RegisterClass RC>:
	FA<op, (outs GPROut:$ra), (ins RC:$rb, ImmOpnd:$shamt),
		!strconcat(instr_asm, "\t$ra, $rb, $shamt"),
		[(set GPROut:$ra, (OpNode RC:$rb, PF:$shamt))], IIAlu> {
	let rc = 0;
}

class shift_rotate_imm32<bits<8> op, bits<4> isRotate, string instr_asm,
							SDNode OpNode>:
	shift_rotate_imm<op, isRotate, instr_asm, OpNode, immZExt5, shamt, CPURegs>;

class shift_rotate_reg<bits<8> op, bits<4> isRotate, string instr_asm,
							SDNode OpNode, RegisterClass RC>:
	FA<op, (outs GPROut:$ra), (ins RC:$rb, RC:$rc),
		!strconcat(instr_asm, "\t$ra, $rb, $rc"),
		[(set GPROut:$ra, (OpNode RC:$rb, RC:$rc))], IIAlu> {
	let shamt = 0;
}

class Mult<bits<8> op, string instr_asm, InstrItinClass itin,
			RegisterClass RC, list<Register> DefRegs>:
	FA<op, (outs), (ins RC:$ra, RC:$rb),
		!strconcat(instr_asm, "\t$ra, $rb"), [], itin>  {
	let rc = 0;
	let shamt = 0;
	let isCommutable = 1;
	let Defs = DefRegs;
	let hasSideEffects = 0;
}

class Mult32<bits<8> op, string instr_asm, InstrItinClass itin>:
	Mult<op, instr_asm, itin, CPURegs, [HI, LO]>;

class Div<SDNode opNode, bits<8> op, string instr_asm, InstrItinClass itin,
			RegisterClass RC, list<Register> DefRegs>:
	FA<op, (outs), (ins RC:$ra, RC:$rb),
		!strconcat(instr_asm, "\t$ra, $rb"),[(opNode RC:$ra, RC:$rb)], itin> {
	let rc = 0;
	let shamt = 0;
	let Defs = DefRegs;
}

class Div32<SDNode opNode, bits<8> op, string instr_asm, InstrItinClass itin>:
	Div<opNode, op, instr_asm, itin, CPURegs, [HI, LO]>;

class MoveFromLOHI<bits<8> op, string instr_asm, RegisterClass RC,
					list<Register> UseRegs>:
	FA<op, (outs RC:$ra), (ins),
		!strconcat(instr_asm, "\t$ra"), [], IIHiLo> {
	let rb = 0;
	let rc = 0;
	let shamt = 0;
	let Uses = UseRegs;
	let hasSideEffects = 0;
}

class MoveToLOHI<bits<8> op, string instr_asm, RegisterClass RC,
					list<Register> DefRegs>:
	FA<op, (outs), (ins RC:$ra),
		!strconcat(instr_asm, "\t$ra"), [], IIHiLo> {
	let rb = 0;
	let rc = 0;
	let shamt = 0;
	let Defs = DefRegs;
	let hasSideEffects = 0;
}

class MoveFromC0<bits<8> op, string instr_asm, RegisterClass RC>:
	FA<op, (outs), (ins RC:$ra, C0Regs:$rb),
		!strconcat(instr_asm, "\t$ra, $rb"), [], IIAlu> {
	let rc = 0;
	let shamt = 0;
	let hasSideEffects = 0;
}

class MoveToC0<bits<8> op, string instr_asm, RegisterClass RC>:
	FA<op, (outs C0Regs:$ra), (ins RC:$rb),
		!strconcat(instr_asm, "\t$ra, $rb"), [], IIAlu> {
	let rc = 0;
	let shamt = 0;
	let hasSideEffects = 0;
}

class C0Move<bits<8> op, string instr_asm>:
	FA<op, (outs C0Regs:$ra), (ins C0Regs:$rb),
		!strconcat(instr_asm, "\t$ra, $rb"), [], IIAlu> {
	let rc = 0;
	let shamt = 0;
	let hasSideEffects = 0;
}

let isReturn=1, isTerminator=1, hasDelaySlot=1, isBarrier=1, hasCtrlDep=1 in
	def RetLR : SampleCGPseudo<(outs), (ins), "", [(SampleCGRet)]>;
  
//===----------------------------------------------------------------------===//
// Instruction definition
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// SampleCG Instructions
//===----------------------------------------------------------------------===//

/// Load and Store Instructions
///  aligned
defm LD     : LoadM32<0x01,  "ld",  load_a>;
defm ST     : StoreM32<0x02, "st",  store_a>;

/// Arithmetic Instructions (ALU Immediate)
// IR "add" defined in include/llvm/Target/TargetSelectionDAG.td, line 315 (def add).
def ADDiu   : ArithLogicI<0x09, "addiu", add, simm16, immSExt16, CPURegs>;

/// Arithmetic Instructions (3-Operand, R-Type)

/// Shift Instructions

def JR      : JumpFR<0x3c, "jr", GPROut>;

def RET     : RetBase<GPROut>;

/// No operation
let addr=0 in
  def NOP   : FJ<0, (outs), (ins), "nop", [], IIAlu>;

//===----------------------------------------------------------------------===//
//  Arbitrary patterns that map to one or more instructions
//===----------------------------------------------------------------------===//

def EnableOverflow : Predicate<"Subtarget->enableOverflow()">;
def DisableOverflow : Predicate<"Subtarget->disableOverflow()">;

def HasCmp : Predicate<"Subtarget->hasCmp()">;
def HasSlt : Predicate<"Subtarget->hasSlt()">;

def ORi : ArithLogicI<0xd, "ori", or, uimm16, immZExt16, CPURegs>;
def LUi : LoadUpper<0xf, "lui", GPROut, uimm16>;

let Predicates = [DisableOverflow] in{
def ADDu : ArithLogicR<0x11, "addu", add, IIAlu, CPURegs, 1>;
def SUBu : ArithLogicR<0x12, "subu", sub, IIAlu, CPURegs>;
}

def SHL : shift_rotate_imm32<0x1e, 0x00, "shl", shl>;

let Predicates = [EnableOverflow] in{
def ADD : ArithLogicR<0x13, "add", add, IIAlu, CPURegs, 1>;
def SUB : ArithLogicR<0x14, "sub", sub, IIAlu, CPURegs>;
}

def MUL : ArithLogicR<0x17, "mul", mul, IIImul, CPURegs, 1>;

def ROL : shift_rotate_imm32<0x1b, 0x01, "rol", rotl>;
def ROR : shift_rotate_imm32<0x1c, 0x01, "ror", rotr>;
def SRA  : shift_rotate_imm32<0x1d, 0x00, "sra", sra>;

def SHR : shift_rotate_imm32<0x1f, 0x00, "shr", srl>;
def SRAV : shift_rotate_reg<0x20, 0x00, "srav", sra, CPURegs>;
def SHLV : shift_rotate_reg<0x21, 0x00, "shlv", shl, CPURegs>;
def SHRV : shift_rotate_reg<0x22, 0x00, "shrv", srl, CPURegs>;
def ROLV : shift_rotate_reg<0x23, 0x01, "rolv", rotl, CPURegs>;
def RORV : shift_rotate_reg<0x24, 0x01, "rorv", rotr, CPURegs>;

def MULT : Mult32<0x41, "mult", IIImul>;
def MULTu : Mult32<0x42, "multu", IIImul>;
def SDIV : Div32<SampleCGDivRem, 0x43, "div", IIIdiv>;
def UDIV : Div32<SampleCGDivRemU, 0x44, "divu", IIIdiv>;
def MFHI : MoveFromLOHI<0x46, "mfhi", CPURegs, [HI]>;
def MFLO : MoveFromLOHI<0x47, "mflo", CPURegs, [LO]>;
def MTHI : MoveToLOHI<0x48, "mthi", CPURegs, [HI]>;
def MTLO : MoveToLOHI<0x49, "mtlo", CPURegs, [LO]>;
def MFC0 : MoveFromC0<0x50, "mfc0", CPURegs>;
def MTC0 : MoveToC0<0x51, "mtc0", CPURegs>;
def C0MOVE : C0Move<0x52, "c0mov">;

def ANDi    : ArithLogicI<0x0c, "andi",and, uimm16, immZExt16, CPURegs>;
def XORi    : ArithLogicI<0x0e, "xori", xor, uimm16, immZExt16, CPURegs>;
def AND     : ArithLogicR<0x18, "and",and, IIAlu, CPURegs, 1>;
def OR      : ArithLogicR<0x19, "or",or, IIAlu, CPURegs, 1>;
def XOR     : ArithLogicR<0x1a, "xor", xor, IIAlu, CPURegs, 1>;

def : Pat<(not CPURegs:$in),
// 1: in == 0; 0: in != 0
	(XORi CPURegs:$in, 1)>;

// Small immediates
def : Pat<(i32 immSExt16:$in),
          (ADDiu ZERO, imm:$in)>;

def : Pat<(i32 immZExt16:$in),
			(ORi ZERO, imm:$in)>;

def : Pat<(i32 immLow16Zero:$in),
			(LUi (HI16 imm:$in))>;

// Arbitrary immediates
def : Pat<(i32 imm:$imm),
			(ORi (LUi (HI16 imm:$imm)), (LO16 imm:$imm))>;

class CmpInstr<bits<8> op, string instr_asm,
				InstrItinClass itin, RegisterClass RC, RegisterClass RD,
				bit isComm = 0>:
	FA<op, (outs RD:$ra), (ins RC:$rb, RC:$rc),
		!strconcat(instr_asm, "\t$ra, $rb, $rc"), [], itin> {
	let shamt = 0;
	let isCommutable = isComm;
	let Predicates = [HasCmp];
}

def CMP     : CmpInstr<0x10, "cmp", IIAlu, CPURegs, SR, 0>;

multiclass SeteqPatsCmp<RegisterClass RC> {
// a == b
	def : Pat<(seteq RC:$lhs, RC:$rhs),
			(SHR (ANDi (CMP RC:$lhs, RC:$rhs), 2), 1)>;
// a != b
	def : Pat<(setne RC:$lhs, RC:$rhs),
			(XORi (SHR (ANDi (CMP RC:$lhs, RC:$rhs), 2), 1), 1)>;
}

multiclass SetltPatsCmp<RegisterClass RC> {
	def : Pat<(setlt RC:$lhs, RC:$rhs),
				(ANDi (CMP RC:$lhs, RC:$rhs), 1)>;
// if cpu0  `define N    `SW[31]  instead of `SW[0]
// Negative flag, then need
// 2 more instructions as follows,
//          (XORi (ANDi (SHR (CMP RC:$lhs, RC:$rhs), (LUi 0x8000), 31), 1), 1)>;

	def : Pat<(setult RC:$lhs, RC:$rhs),
			(ANDi (CMP RC:$lhs, RC:$rhs), 1)>;
}

multiclass SetlePatsCmp<RegisterClass RC> {
	def : Pat<(setle RC:$lhs, RC:$rhs),
// a <= b is equal to (XORi (b < a), 1)
		(XORi (ANDi (CMP RC:$rhs, RC:$lhs), 1), 1)>;
	def : Pat<(setule RC:$lhs, RC:$rhs),
		(XORi (ANDi (CMP RC:$rhs, RC:$lhs), 1), 1)>;
}

multiclass SetgtPatsCmp<RegisterClass RC> {
	def : Pat<(setgt RC:$lhs, RC:$rhs),
// a > b is equal to b < a is equal to setlt(b, a)
			(ANDi (CMP RC:$rhs, RC:$lhs), 1)>;
	def : Pat<(setugt RC:$lhs, RC:$rhs),
			(ANDi (CMP RC:$rhs, RC:$lhs), 1)>;
}

multiclass SetgePatsCmp<RegisterClass RC> {
	def : Pat<(setge RC:$lhs, RC:$rhs),
// a >= b is equal to b <= a
		(XORi (ANDi (CMP RC:$lhs, RC:$rhs), 1), 1)>;
	def : Pat<(setuge RC:$lhs, RC:$rhs),
		(XORi (ANDi (CMP RC:$lhs, RC:$rhs), 1), 1)>;
}

class SetCC_I<bits<8> op, string instr_asm, PatFrag cond_op, Operand Od,
			PatLeaf imm_type, RegisterClass RC>:
	FL<op, (outs GPROut:$ra), (ins RC:$rb, Od:$imm16),
		!strconcat(instr_asm, "\t$ra, $rb, $imm16"),
		[(set GPROut:$ra, (cond_op RC:$rb, imm_type:$imm16))],
		IIAlu>, Requires<[HasSlt]> {
}

class SetCC_R<bits<8> op, string instr_asm, PatFrag cond_op,
					RegisterClass RC>
	:FA<op, (outs GPROut:$ra), (ins RC:$rb, RC:$rc),
		!strconcat(instr_asm, "\t$ra, $rb, $rc"),
		[(set GPROut:$ra, (cond_op RC:$rb, RC:$rc))],
		IIAlu>, Requires<[HasSlt]> {
	let shamt = 0;
}


multiclass SeteqPatsSlt<RegisterClass RC, Instruction SLTiuOp, Instruction XOROp,
					Instruction SLTuOp, Register ZEROReg> {
// a == b
	def : Pat<(seteq RC:$lhs, RC:$rhs),
				(SLTiuOp (XOROp RC:$lhs, RC:$rhs), 1)>;

// a != b
	def : Pat<(setne RC:$lhs, RC:$rhs),
				(SLTuOp ZEROReg, (XOROp RC:$lhs, RC:$rhs))>;
}

multiclass SetlePatsSlt<RegisterClass RC, Instruction SLTOp, Instruction SLTuOp> {
	def : Pat<(setle RC:$lhs, RC:$rhs),
// a <= b is equal to (XORi (b < a), 1)
				(XORi (SLTOp RC:$rhs, RC:$lhs), 1)>;
	def : Pat<(setule RC:$lhs, RC:$rhs),
				(XORi (SLTuOp RC:$rhs, RC:$lhs), 1)>;
}

multiclass SetgtPatsSlt<RegisterClass RC, Instruction SLTOp, Instruction SLTuOp> {
	def : Pat<(setgt RC:$lhs, RC:$rhs),
// a > b is equal to b < a is equal to setlt(b, a)
				(SLTOp RC:$rhs, RC:$lhs)>;
	def : Pat<(setugt RC:$lhs, RC:$rhs),
				(SLTuOp RC:$rhs, RC:$lhs)>;
}

multiclass SetgePatsSlt<RegisterClass RC, Instruction SLTOp, Instruction SLTuOp> {
	def : Pat<(setge RC:$lhs, RC:$rhs),
// a >= b is equal to b <= a
				(XORi (SLTOp RC:$lhs, RC:$rhs), 1)>;
	def : Pat<(setuge RC:$lhs, RC:$rhs),
				(XORi (SLTuOp RC:$lhs, RC:$rhs), 1)>;
}

multiclass SetgeImmPatsSlt<RegisterClass RC, Instruction SLTiOp,
							Instruction SLTiuOp> {
	def : Pat<(setge RC:$lhs, immSExt16:$rhs),
				(XORi (SLTiOp RC:$lhs, immSExt16:$rhs), 1)>;
	def : Pat<(setuge RC:$lhs, immSExt16:$rhs),
				(XORi (SLTiuOp RC:$lhs, immSExt16:$rhs), 1)>;
}


let Predicates = [HasSlt] in {
def SLTi    : SetCC_I<0x26, "slti", setlt, simm16, immSExt16, CPURegs>;
def SLTiu   : SetCC_I<0x27, "sltiu", setult, simm16, immSExt16, CPURegs>;
def SLT     : SetCC_R<0x28, "slt", setlt, CPURegs>;
def SLTu    : SetCC_R<0x29, "sltu", setult, CPURegs>;
}

let Predicates = [HasSlt] in {
defm : SeteqPatsSlt<CPURegs, SLTiu, XOR, SLTu, ZERO>;
defm : SetlePatsSlt<CPURegs, SLT, SLTu>;
defm : SetgtPatsSlt<CPURegs, SLT, SLTu>;
defm : SetgePatsSlt<CPURegs, SLT, SLTu>;
defm : SetgeImmPatsSlt<CPURegs, SLTi, SLTiu>;
}

let Predicates = [HasCmp] in {
defm : SeteqPatsCmp<CPURegs>;
defm : SetltPatsCmp<CPURegs>;
defm : SetlePatsCmp<CPURegs>;
defm : SetgtPatsCmp<CPURegs>;
defm : SetgePatsCmp<CPURegs>;
}

def SampleCGHi : SDNode<"SampleCGISD::Hi", SDTIntUnaryOp>;
def SampleCGLo : SDNode<"SampleCGISD::Lo", SDTIntUnaryOp>;
def SampleCGGPRel : SDNode<"SampleCGISD::GPRel", SDTIntUnaryOp>;
def SampleCGWrapper : SDNode<"SampleCGISD::Wrapper", SDTIntBinOp>;
def RelocPIC : Predicate<"TM.getRelocationModel() == Reloc::PIC_">;

def : Pat<(SampleCGHi tglobaladdr:$in), (LUi tglobaladdr:$in)>;
def : Pat<(SampleCGLo tglobaladdr:$in), (ORi ZERO, tglobaladdr:$in)>;
def : Pat<(add CPURegs:$hi, (SampleCGLo tglobaladdr:$lo)),(ORi CPURegs:$hi, tglobaladdr:$lo)>;
def : Pat<(add CPURegs:$gp, (SampleCGGPRel tglobaladdr:$in)),(ORi CPURegs:$gp, tglobaladdr:$in)>;

def : Pat<(SampleCGHi tblockaddress:$in), (LUi tblockaddress:$in)>;
def : Pat<(SampleCGHi tjumptable:$in), (LUi tjumptable:$in)>;
def : Pat<(SampleCGLo tblockaddress:$in), (ORi ZERO, tblockaddress:$in)>;
def : Pat<(SampleCGLo tjumptable:$in), (ORi ZERO, tjumptable:$in)>;

def : Pat<(add CPURegs:$hi, (SampleCGLo tblockaddress:$lo)),
              (ORi CPURegs:$hi, tblockaddress:$lo)>;
def : Pat<(add CPURegs:$hi, (SampleCGLo tjumptable:$lo)),
              (ORi CPURegs:$hi, tjumptable:$lo)>;

class WrapperPat<SDNode node, Instruction ORiOp, RegisterClass RC>:
	Pat<(SampleCGWrapper RC:$gp, node:$in),
	(ORiOp RC:$gp, node:$in)>;
	
def : WrapperPat<tglobaladdr, ORi, GPROut>;

def mem_ea : Operand<iPTR> {
	let PrintMethod = "printMemOperandEA";
	let MIOperandInfo = (ops CPURegs, simm16);
	let EncoderMethod = "getMemEncoding";
}

class EffectiveAddress<string instr_asm, RegisterClass RC, Operand Mem> :
	FMem<0x09, (outs RC:$ra),
		(ins Mem:$addr),instr_asm, [(set RC:$ra, addr:$addr)], IIAlu>;

def LEA_ADDiu : EffectiveAddress<"addiu\t$ra, $addr", CPURegs, mem_ea> {
	let isCodeGenOnly = 1;
}

defm LB : LoadM32<0x03, "lb",  sextloadi8>;
defm LBu : LoadM32<0x04, "lbu", zextloadi8>;
defm SB  : StoreM32<0x05, "sb", truncstorei8>;
defm LH  : LoadM32<0x06, "lh",  sextloadi16_a>;
defm LHu : LoadM32<0x07, "lhu", zextloadi16_a>;
defm SH  : StoreM32<0x08, "sh", truncstorei16_a>;

def : Pat<(i32 (extloadi1  addr:$src)), (LBu addr:$src)>;
def : Pat<(i32 (extloadi8  addr:$src)), (LBu addr:$src)>;
def : Pat<(i32 (extloadi16_a addr:$src)), (LHu addr:$src)>;

class CountLeading0<bits<8> op, string instr_asm, RegisterClass RC>:
	FA<op, (outs GPROut:$ra), (ins RC:$rb),
		!strconcat(instr_asm, "\t$ra, $rb"),
		[(set GPROut:$ra, (ctlz RC:$rb))], II_CLZ> {
	let rc = 0;
	let shamt = 0;
}

class CountLeading1<bits<8> op, string instr_asm, RegisterClass RC>:
	FA<op, (outs GPROut:$ra), (ins RC:$rb),
		!strconcat(instr_asm, "\t$ra, $rb"),
		[(set GPROut:$ra, (ctlz (not RC:$rb)))], II_CLO> {
	let rc = 0;
	let shamt = 0;
}

def CLZ : CountLeading0<0x15, "clz", CPURegs>;
def CLO : CountLeading1<0x16, "clo", CPURegs>;

// BEQ, BNE
def brtarget16    : Operand<OtherVT> {
  let EncoderMethod = "getBranch16TargetOpValue";
  let OperandType = "OPERAND_PCREL";
  let DecoderMethod = "DecodeBranch16Target";
}

// JEQ, JNE, ...
def brtarget24    : Operand<OtherVT> {
  let EncoderMethod = "getBranch24TargetOpValue";
  let OperandType = "OPERAND_PCREL";
  let DecoderMethod = "DecodeBranch24Target";
}

// JMP
def jmptarget    : Operand<OtherVT> {
  let EncoderMethod = "getJumpTargetOpValue";
  let OperandType = "OPERAND_PCREL";
}

class UncondBranch<bits<8> op, string instr_asm>:
	FJ<op, (outs), (ins jmptarget:$addr),
		!strconcat(instr_asm, "\t$addr"), [(br bb:$addr)], IIBranch> {
	let isBranch = 1;
	let isTerminator = 1;
	let isBarrier = 1;
	let hasDelaySlot = 0;
}

class CBranch24<bits<8> op, string instr_asm, RegisterClass RC,
                   list<Register> UseRegs>:
  FJ<op, (outs), (ins RC:$ra, brtarget24:$addr),
             !strconcat(instr_asm, "\t$ra, $addr"),
             [], IIBranch>, Requires<[HasCmp]> {
  let isBranch = 1;
  let isTerminator = 1;
  let hasDelaySlot = 1;
//  let Predicates = [HasCmp]; // same effect as Requires
}

class CBranch16<bits<8> op, string instr_asm, PatFrag cond_op, RegisterClass RC>:
  FL<op, (outs), (ins RC:$ra, RC:$rb, brtarget16:$imm16),
             !strconcat(instr_asm, "\t$ra, $rb, $imm16"),
             [(brcond (i32 (cond_op RC:$ra, RC:$rb)), bb:$imm16)], IIBranch>,
             Requires<[HasSlt]> {
  let isBranch = 1;
  let isTerminator = 1;
  let hasDelaySlot = 1;
  let Defs = [AT];
}

def JMP : UncondBranch<0x36, "jmp">;

let Predicates = [HasCmp] in {
def JEQ     : CBranch24<0x30, "jeq", SR, [SW]>;
def JNE     : CBranch24<0x31, "jne", SR, [SW]>;
def JLT     : CBranch24<0x32, "jlt", SR, [SW]>;
def JGT     : CBranch24<0x33, "jgt", SR, [SW]>;
def JLE     : CBranch24<0x34, "jle", SR, [SW]>;
def JGE     : CBranch24<0x35, "jge", SR, [SW]>;
}

let Predicates = [HasSlt] in {
def BEQ     : CBranch16<0x37, "beq", seteq, GPROut>;
def BNE     : CBranch16<0x38, "bne", setne, GPROut>;
}

multiclass BrcondPatsCmp<RegisterClass RC, Instruction JEQOp, Instruction JNEOp,
Instruction JLTOp, Instruction JGTOp, Instruction JLEOp, Instruction JGEOp,
Instruction CMPOp, Register ZEROReg> {
def : Pat<(brcond (i32 (seteq RC:$lhs, RC:$rhs)), bb:$dst),
          (JEQOp (CMPOp RC:$lhs, RC:$rhs), bb:$dst)>;
def : Pat<(brcond (i32 (setueq RC:$lhs, RC:$rhs)), bb:$dst),
          (JEQOp (CMPOp RC:$lhs, RC:$rhs), bb:$dst)>;
def : Pat<(brcond (i32 (setne RC:$lhs, RC:$rhs)), bb:$dst),
          (JNEOp (CMPOp RC:$lhs, RC:$rhs), bb:$dst)>;
def : Pat<(brcond (i32 (setune RC:$lhs, RC:$rhs)), bb:$dst),
          (JNEOp (CMPOp RC:$lhs, RC:$rhs), bb:$dst)>;
def : Pat<(brcond (i32 (setlt RC:$lhs, RC:$rhs)), bb:$dst),
          (JLTOp (CMPOp RC:$lhs, RC:$rhs), bb:$dst)>;
def : Pat<(brcond (i32 (setult RC:$lhs, RC:$rhs)), bb:$dst),
          (JLTOp (CMPOp RC:$lhs, RC:$rhs), bb:$dst)>;
def : Pat<(brcond (i32 (setgt RC:$lhs, RC:$rhs)), bb:$dst),
          (JGTOp (CMPOp RC:$lhs, RC:$rhs), bb:$dst)>;
def : Pat<(brcond (i32 (setugt RC:$lhs, RC:$rhs)), bb:$dst),
          (JGTOp (CMPOp RC:$lhs, RC:$rhs), bb:$dst)>;
def : Pat<(brcond (i32 (setle RC:$lhs, RC:$rhs)), bb:$dst),
          (JLEOp (CMPOp RC:$lhs, RC:$rhs), bb:$dst)>;
def : Pat<(brcond (i32 (setule RC:$lhs, RC:$rhs)), bb:$dst),
          (JLEOp (CMPOp RC:$lhs, RC:$rhs), bb:$dst)>;
def : Pat<(brcond (i32 (setge RC:$lhs, RC:$rhs)), bb:$dst),
          (JGEOp (CMPOp RC:$lhs, RC:$rhs), bb:$dst)>;
def : Pat<(brcond (i32 (setuge RC:$lhs, RC:$rhs)), bb:$dst),
          (JGEOp (CMPOp RC:$lhs, RC:$rhs), bb:$dst)>;

def : Pat<(brcond RC:$cond, bb:$dst),
          (JNEOp (CMPOp RC:$cond, ZEROReg), bb:$dst)>;
}


// brcond for slt instruction
multiclass BrcondPatsSlt<RegisterClass RC, Instruction BEQOp, Instruction BNEOp,
                      Instruction SLTOp, Instruction SLTuOp, Instruction SLTiOp,
                      Instruction SLTiuOp, Register ZEROReg> {
def : Pat<(brcond (i32 (setne RC:$lhs, 0)), bb:$dst),
              (BNEOp RC:$lhs, ZEROReg, bb:$dst)>;
def : Pat<(brcond (i32 (seteq RC:$lhs, 0)), bb:$dst),
              (BEQOp RC:$lhs, ZEROReg, bb:$dst)>;

def : Pat<(brcond (i32 (seteq RC:$lhs, RC:$rhs)), bb:$dst),
              (BEQOp RC:$lhs, RC:$rhs, bb:$dst)>;
def : Pat<(brcond (i32 (setueq RC:$lhs, RC:$rhs)), bb:$dst),
              (BEQOp RC:$lhs, RC:$rhs, bb:$dst)>;
def : Pat<(brcond (i32 (setne RC:$lhs, RC:$rhs)), bb:$dst),
              (BNEOp RC:$lhs, RC:$rhs, bb:$dst)>;
def : Pat<(brcond (i32 (setune RC:$lhs, RC:$rhs)), bb:$dst),
              (BNEOp RC:$lhs, RC:$rhs, bb:$dst)>;
def : Pat<(brcond (i32 (setlt RC:$lhs, RC:$rhs)), bb:$dst),
              (BNE (SLTOp RC:$lhs, RC:$rhs), ZERO, bb:$dst)>;
def : Pat<(brcond (i32 (setult RC:$lhs, RC:$rhs)), bb:$dst),
              (BNE (SLTuOp RC:$lhs, RC:$rhs), ZERO, bb:$dst)>;
def : Pat<(brcond (i32 (setgt RC:$lhs, RC:$rhs)), bb:$dst),
              (BNE (SLTOp RC:$rhs, RC:$lhs), ZERO, bb:$dst)>;
def : Pat<(brcond (i32 (setugt RC:$lhs, RC:$rhs)), bb:$dst),
              (BNE (SLTuOp RC:$rhs, RC:$lhs), ZERO, bb:$dst)>;
def : Pat<(brcond (i32 (setle RC:$lhs, RC:$rhs)), bb:$dst),
              (BEQ (SLTOp RC:$rhs, RC:$lhs), ZERO, bb:$dst)>;
def : Pat<(brcond (i32 (setule RC:$lhs, RC:$rhs)), bb:$dst),
              (BEQ (SLTuOp RC:$rhs, RC:$lhs), ZERO, bb:$dst)>;
def : Pat<(brcond (i32 (setge RC:$lhs, RC:$rhs)), bb:$dst),
              (BEQ (SLTOp RC:$lhs, RC:$rhs), ZERO, bb:$dst)>;
def : Pat<(brcond (i32 (setuge RC:$lhs, RC:$rhs)), bb:$dst),
              (BEQ (SLTuOp RC:$lhs, RC:$rhs), ZERO, bb:$dst)>;

def : Pat<(brcond RC:$cond, bb:$dst),
              (BNEOp RC:$cond, ZEROReg, bb:$dst)>;
}

let Predicates = [HasSlt] in {
defm : BrcondPatsSlt<CPURegs, BEQ, BNE, SLT, SLTu, SLTi, SLTiu, ZERO>;
}

let Predicates = [HasCmp] in {
defm : BrcondPatsCmp<CPURegs, JEQ, JNE, JLT, JGT, JLE, JGE, CMP, ZERO>;
}


def LONG_BRANCH_LUi : SampleCGPseudo<(outs GPROut:$dst),
	(ins jmptarget:$tgt, jmptarget:$baltgt), "", []>;

// Expands to: addiu $dst, $src, %lo($tgt - $baltgt)
def LONG_BRANCH_ADDiu : SampleCGPseudo<(outs GPROut:$dst),
	(ins GPROut:$src, jmptarget:$tgt, jmptarget:$baltgt), "", []>;

let isBranch = 1, isTerminator = 1, isBarrier = 1,
	hasDelaySlot = 0, Defs = [LR] in
def BAL: FJ<0x3A, (outs), (ins jmptarget:$addr), "bal\t$addr", [], IIBranch>,
	Requires<[HasSlt]>;

// Tail call

def SampleCGTailCall : SDNode<"SampleCGISD::TailCall", SDT_SampleCGJmpLink,
	[SDNPHasChain, SDNPOptInGlue, SDNPVariadic]>;


let isCall = 1, isTerminator = 1, isReturn = 1, isBarrier = 1, hasDelaySlot = 1,
	hasExtraSrcRegAllocReq = 1, Defs = [AT] in {
	class TailCall<Instruction JumpInst> :
		PseudoSE<(outs), (ins calltarget:$target), [], IIBranch>,
		PseudoInstExpansion<(JumpInst jmptarget:$target)>;
		
	class TailCallReg<RegisterClass RO, Instruction JRInst,
		RegisterClass ResRO = RO> :
		PseudoSE<(outs), (ins RO:$rs), [(SampleCGTailCall RO:$rs)], IIBranch>,
		PseudoInstExpansion<(JRInst ResRO:$rs)>;
}

def TAILCALL : TailCall<JMP>;
def TAILCALL_R : TailCallReg<GPROut, JR>;

def : Pat<(SampleCGTailCall (iPTR tglobaladdr:$dst)),
	(TAILCALL tglobaladdr:$dst)>;

class SampleCGInstAlias<string Asm, dag Result, bit Emit = 0b1> :
  InstAlias<Asm, Result, Emit>;

//===----------------------------------------------------------------------===//
// Instruction aliases
//===----------------------------------------------------------------------===//
def : SampleCGInstAlias<"move $dst, $src",
                    (ADDu GPROut:$dst, GPROut:$src,ZERO), 1>;

// Exception handling related node and instructions.
// The conversion sequence is:
// ISD::EH_RETURN -> SampleCGISD::EH_RETURN ->
// SAMPLECGeh_return -> (stack change + indirect branch)
//
// SAMPLECGeh_return takes the place of regular return instruction
// but takes two arguments (V1, V0) which are used for storing
// the offset and return address respectively.
def SDT_SampleCGEHRET : SDTypeProfile<0, 2, [SDTCisInt<0>, SDTCisPtrTy<1>]>;

def SAMPLECGehret : SDNode<"SampleCGISD::EH_RETURN", SDT_SampleCGEHRET,
                      [SDNPHasChain, SDNPOptInGlue, SDNPVariadic]>;

let Uses = [V0, V1], isTerminator = 1, isReturn = 1, isBarrier = 1 in {
  def SAMPLECGeh_return32 : SampleCGPseudo<(outs), (ins GPROut:$spoff, GPROut:$dst), "",
                                [(SAMPLECGehret GPROut:$spoff, GPROut:$dst)]>;

}

class LoadImm32< string instr_asm, Operand Od, RegisterClass RC> :
  SampleCGAsmPseudoInst<(outs RC:$ra), (ins Od:$imm32),
                     !strconcat(instr_asm, "\t$ra, $imm32")> ;
def LoadImm32Reg : LoadImm32<"li", shamt, GPROut>;

class LoadAddress<string instr_asm, Operand MemOpnd, RegisterClass RC> :
  SampleCGAsmPseudoInst<(outs RC:$ra), (ins MemOpnd:$addr),
                     !strconcat(instr_asm, "\t$ra, $addr")> ;
def LoadAddr32Reg : LoadAddress<"la", mem, GPROut>;

class LoadAddressImm<string instr_asm, Operand Od, RegisterClass RC> :
  SampleCGAsmPseudoInst<(outs RC:$ra), (ins Od:$imm32),
                     !strconcat(instr_asm, "\t$ra, $imm32")> ;
def LoadAddr32Imm : LoadAddressImm<"la", shamt, GPROut>;

// TlsGd node is used to handle General Dynamic TLS
def SampleCGTlsGd : SDNode<"SampleCGISD::TlsGd", SDTIntUnaryOp>;

// TpHi and TpLo nodes are used to handle Local Exec TLS
def SampleCGTpHi  : SDNode<"SampleCGISD::TpHi", SDTIntUnaryOp>;
def SampleCGTpLo  : SDNode<"SampleCGISD::TpLo", SDTIntUnaryOp>;
def : Pat<(SampleCGHi tglobaltlsaddr:$in), (LUi tglobaltlsaddr:$in)>;
def : Pat<(SampleCGLo tglobaltlsaddr:$in), (ORi ZERO, tglobaltlsaddr:$in)>;
def : Pat<(add CPURegs:$hi, (SampleCGLo tglobaltlsaddr:$lo)),
              (ORi CPURegs:$hi, tglobaltlsaddr:$lo)>;
def : WrapperPat<tglobaltlsaddr, ORi, CPURegs>;

def SDT_Sync             : SDTypeProfile<0, 1, [SDTCisVT<0, i32>]>;
def SampleCGSync : SDNode<"SampleCGISD::Sync", SDT_Sync, [SDNPHasChain]>;

def PtrRC : Operand<iPTR> {
  let MIOperandInfo = (ops ptr_rc);
  let DecoderMethod = "DecodeCPURegsRegisterClass";
}

// Atomic instructions with 2 source operands (ATOMIC_SWAP & ATOMIC_LOAD_*).
class Atomic2Ops<PatFrag Op, RegisterClass DRC> :
  PseudoSE<(outs DRC:$dst), (ins PtrRC:$ptr, DRC:$incr),
           [(set DRC:$dst, (Op iPTR:$ptr, DRC:$incr))]>;

// Atomic Compare & Swap.
class AtomicCmpSwap<PatFrag Op, RegisterClass DRC> :
  PseudoSE<(outs DRC:$dst), (ins PtrRC:$ptr, DRC:$cmp, DRC:$swap),
           [(set DRC:$dst, (Op iPTR:$ptr, DRC:$cmp, DRC:$swap))]>;

class LLBase<bits<8> Opc, string opstring, RegisterClass RC, Operand Mem> :
  FMem<Opc, (outs RC:$ra), (ins Mem:$addr),
       !strconcat(opstring, "\t$ra, $addr"), [], IILoad> {
  let mayLoad = 1;
}

class SCBase<bits<8> Opc, string opstring, RegisterOperand RO, Operand Mem> :
  FMem<Opc, (outs RO:$dst), (ins RO:$ra, Mem:$addr),
       !strconcat(opstring, "\t$ra, $addr"), [], IIStore> {
  let mayStore = 1;
  let Constraints = "$ra = $dst";
}

let usesCustomInserter = 1 in {
  def ATOMIC_LOAD_ADD_I8   : Atomic2Ops<atomic_load_add_8, CPURegs>;
  def ATOMIC_LOAD_ADD_I16  : Atomic2Ops<atomic_load_add_16, CPURegs>;
  def ATOMIC_LOAD_ADD_I32  : Atomic2Ops<atomic_load_add_32, CPURegs>;
  def ATOMIC_LOAD_SUB_I8   : Atomic2Ops<atomic_load_sub_8, CPURegs>;
  def ATOMIC_LOAD_SUB_I16  : Atomic2Ops<atomic_load_sub_16, CPURegs>;
  def ATOMIC_LOAD_SUB_I32  : Atomic2Ops<atomic_load_sub_32, CPURegs>;
  def ATOMIC_LOAD_AND_I8   : Atomic2Ops<atomic_load_and_8, CPURegs>;
  def ATOMIC_LOAD_AND_I16  : Atomic2Ops<atomic_load_and_16, CPURegs>;
  def ATOMIC_LOAD_AND_I32  : Atomic2Ops<atomic_load_and_32, CPURegs>;
  def ATOMIC_LOAD_OR_I8    : Atomic2Ops<atomic_load_or_8, CPURegs>;
  def ATOMIC_LOAD_OR_I16   : Atomic2Ops<atomic_load_or_16, CPURegs>;
  def ATOMIC_LOAD_OR_I32   : Atomic2Ops<atomic_load_or_32, CPURegs>;
  def ATOMIC_LOAD_XOR_I8   : Atomic2Ops<atomic_load_xor_8, CPURegs>;
  def ATOMIC_LOAD_XOR_I16  : Atomic2Ops<atomic_load_xor_16, CPURegs>;
  def ATOMIC_LOAD_XOR_I32  : Atomic2Ops<atomic_load_xor_32, CPURegs>;
  def ATOMIC_LOAD_NAND_I8  : Atomic2Ops<atomic_load_nand_8, CPURegs>;
  def ATOMIC_LOAD_NAND_I16 : Atomic2Ops<atomic_load_nand_16, CPURegs>;
  def ATOMIC_LOAD_NAND_I32 : Atomic2Ops<atomic_load_nand_32, CPURegs>;

  def ATOMIC_SWAP_I8       : Atomic2Ops<atomic_swap_8, CPURegs>;
  def ATOMIC_SWAP_I16      : Atomic2Ops<atomic_swap_16, CPURegs>;
  def ATOMIC_SWAP_I32      : Atomic2Ops<atomic_swap_32, CPURegs>;

  def ATOMIC_CMP_SWAP_I8   : AtomicCmpSwap<atomic_cmp_swap_8, CPURegs>;
  def ATOMIC_CMP_SWAP_I16  : AtomicCmpSwap<atomic_cmp_swap_16, CPURegs>;
  def ATOMIC_CMP_SWAP_I32  : AtomicCmpSwap<atomic_cmp_swap_32, CPURegs>;
}

let hasSideEffects = 1 in
def SYNC : SampleCGInst<(outs), (ins i32imm:$stype), "sync $stype",
                    [(SampleCGSync imm:$stype)], NoItinerary, FrmOther>
{
  bits<5> stype;
  let Opcode = 0x60;
  let Inst{25-11} = 0;
  let Inst{10-6} = stype;
  let Inst{5-0} = 0;
}

/// Load-linked, Store-conditional
def LL      : LLBase<0x61, "ll", CPURegs, mem>;
def SC      : SCBase<0x62, "sc", RegisterOperand<CPURegs>, mem>;

def : SampleCGInstAlias<"sync",
                    (SYNC 0), 1>;


include "SampleCGCondMov.td"


