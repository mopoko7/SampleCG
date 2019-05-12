//===-- SampleCG.td - Describe the SampleCG Target Machine -------*- tablegen -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Target-independent interfaces which we are implementing
//===----------------------------------------------------------------------===//

include "llvm/Target/Target.td"

//===----------------------------------------------------------------------===//
// Register File, Calling Conv, Instruction Descriptions
//===----------------------------------------------------------------------===//

include "SampleCGRegisterInfo.td"
include "SampleCGSchedule.td"
include "SampleCGInstrInfo.td"
include "SampleCGCallingConv.td"

def FeatureCmp	: SubtargetFeature<"cmp", "HasCmp", "true", "Enable 'cmp' instruction.">;
def FeatureSlt	: SubtargetFeature<"slt", "HasSlt", "true", "Enable 'slt' instruction.">;

def FeatureSampleCG32I	: SubtargetFeature<"samplecg32I", "SampleCGArchVersion", "SampleCG32I", "SampleCG32I ISA Support", [FeatureCmp]>;
def FeatureSampleCG32II : SubtargetFeature<"samplecg32II", "SampleCGArchVersion", "SampleCG32II", "SampleCG32II ISA Support", [FeatureCmp, FeatureSlt]>;


def SampleCGInstrInfo : InstrInfo;

def : ProcessorModel<"samplecg", SampleCGSchedModel, []>;
def : ProcessorModel<"samplecg32I", SampleCGSchedModel, []>;
def : ProcessorModel<"samplecg32II", SampleCGSchedModel, []>;

def SampleCGAsmParser : AsmParser {
  let ShouldEmitMatchRegisterName = 0;
  let AllowDuplicateRegisterNames = 1;
}

def SampleCGAsmParserVariant : AsmParserVariant {
	int Variant = 0;
	
	// Recognize hard coded registers.
	string RegisterPrefix = "$";
}

// Will generate SampleCGGenAsmWrite.inc included by SampleCGInstPrinter.cpp, contents
//  as follows,
// void SampleCGInstPrinter::printInstruction(const MCInst *MI, raw_ostream &O) {...}
// const char *SampleCGInstPrinter::getRegisterName(unsigned RegNo) {...}
def SampleCG : Target {
// def SampleCGInstrInfo : InstrInfo as before.
  let InstructionSet = SampleCGInstrInfo;
  let AssemblyParsers = [SampleCGAsmParser];
  let AssemblyParserVariants = [SampleCGAsmParserVariant];
}

