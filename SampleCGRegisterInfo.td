//===-- SampleCGRegisterInfo.td - SampleCG Register defs -----------*- tablegen -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
//  Declarations that describe the SAMPLECG register file
//===----------------------------------------------------------------------===//

// We have banks of 16 registers each.
class SampleCGReg<bits<16> Enc, string n> : Register<n> {
  // For tablegen(... -gen-emitter)  in CMakeLists.txt
  let HWEncoding = Enc;
  
  let Namespace = "SampleCG";
}

// SampleCG CPU Registers
class SampleCGGPRReg<bits<16> Enc, string n> : SampleCGReg<Enc, n>;

// Co-processor 0 Registers
class SampleCGC0Reg<bits<16> Enc, string n> : SampleCGReg<Enc, n>;

//===----------------------------------------------------------------------===//
//@Registers
//===----------------------------------------------------------------------===//
// The register string, such as "9" or "gp" will show on "llvm-objdump -d"
//@ All registers definition
let Namespace = "SampleCG" in {
  //@ General Purpose Registers
  def ZERO : SampleCGGPRReg<0,  "zero">, DwarfRegNum<[0]>;
  def AT   : SampleCGGPRReg<1,  "1">,    DwarfRegNum<[1]>;
  def V0   : SampleCGGPRReg<2,  "2">,    DwarfRegNum<[2]>;
  def V1   : SampleCGGPRReg<3,  "3">,    DwarfRegNum<[3]>;
  def A0   : SampleCGGPRReg<4,  "4">,    DwarfRegNum<[4]>;
  def A1   : SampleCGGPRReg<5,  "5">,    DwarfRegNum<[5]>;
  def T9   : SampleCGGPRReg<6,  "t9">,   DwarfRegNum<[6]>;
  def T0   : SampleCGGPRReg<7,  "7">,    DwarfRegNum<[7]>;
  def T1   : SampleCGGPRReg<8,  "8">,    DwarfRegNum<[8]>;
  def S0   : SampleCGGPRReg<9,  "9">,    DwarfRegNum<[9]>;
  def S1   : SampleCGGPRReg<10, "10">,   DwarfRegNum<[10]>;
  def GP   : SampleCGGPRReg<11, "gp">,   DwarfRegNum<[11]>;
  def FP   : SampleCGGPRReg<12, "fp">,   DwarfRegNum<[12]>;
  def SP   : SampleCGGPRReg<13, "sp">,   DwarfRegNum<[13]>;
  def LR   : SampleCGGPRReg<14, "lr">,   DwarfRegNum<[14]>;
  def SW   : SampleCGGPRReg<15, "sw">,   DwarfRegNum<[15]>;
//  def MAR  : Register< 16, "mar">,  DwarfRegNum<[16]>;
//  def MDR  : Register< 17, "mdr">,  DwarfRegNum<[17]>;

  // Hi/Lo registers number and name
  def HI   : SampleCGReg<0, "ac0hi">, DwarfRegNum<[18]>;
  def LO   : SampleCGReg<0, "ac0lo">, DwarfRegNum<[19]>;
  def PC   : SampleCGC0Reg<0, "pc">,  DwarfRegNum<[20]>;
  def EPC  : SampleCGC0Reg<1, "epc">, DwarfRegNum<[21]>;
}

//===----------------------------------------------------------------------===//
//@Register Classes
//===----------------------------------------------------------------------===//

def CPURegs : RegisterClass<"SampleCG", [i32], 32, (add
  // Reserved
  ZERO, AT, 
  // Return Values and Arguments
  V0, V1, A0, A1, 
  // Not preserved across procedure calls
  T9, T0, T1,
  // Callee save
  S0, S1,
  // Reserved
  GP, FP, 
  SP, LR, SW)>;

// Hi/Lo Registers class
def HILO   : RegisterClass<"SampleCG", [i32], 32, (add HI, LO)>;

//@Status Registers class
def SR     : RegisterClass<"SampleCG", [i32], 32, (add SW)>;

//@Co-processor 0 Registers class
def C0Regs : RegisterClass<"SampleCG", [i32], 32, (add PC, EPC)>;

def GPROut : RegisterClass<"SampleCG", [i32], 32, (add (sub CPURegs, SW))>;
