//===-- SampleCGSchedule.td - SampleCG Scheduling Definitions ------*- tablegen -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Functional units across SampleCG chips sets.
//===----------------------------------------------------------------------===//
def ALU     : FuncUnit;
def IMULDIV : FuncUnit;

//===----------------------------------------------------------------------===//
// Instruction Itinerary classes used for SampleCG
//===----------------------------------------------------------------------===//
def IIAlu              : InstrItinClass;
def II_CLO             : InstrItinClass;
def II_CLZ             : InstrItinClass;
def IILoad             : InstrItinClass;
def IIStore            : InstrItinClass;
def IIBranch           : InstrItinClass;

def IIPseudo           : InstrItinClass;

def IIHiLo : InstrItinClass;
def IIImul : InstrItinClass;
def IIIdiv : InstrItinClass;


//===----------------------------------------------------------------------===//
// SampleCG Generic instruction itineraries.
//===----------------------------------------------------------------------===//
//@ http://llvm.org/docs/doxygen/html/structllvm_1_1InstrStage.html
def SampleCGGenericItineraries : ProcessorItineraries<[ALU, IMULDIV], [], [
//@2
  InstrItinData<IIAlu              , [InstrStage<1,  [ALU]>]>,
  InstrItinData<II_CLO             , [InstrStage<1,  [ALU]>]>,
  InstrItinData<II_CLZ             , [InstrStage<1,  [ALU]>]>,
  InstrItinData<IILoad             , [InstrStage<3,  [ALU]>]>,
  InstrItinData<IIStore            , [InstrStage<1,  [ALU]>]>,
  InstrItinData<IIBranch           , [InstrStage<1,  [ALU]>]>,
  InstrItinData<IIHiLo             , [InstrStage<1,  [IMULDIV]>]>,
  InstrItinData<IIImul             , [InstrStage<17, [IMULDIV]>]>,
  InstrItinData<IIIdiv             , [InstrStage<1, [IMULDIV]>]>
]>;

def SampleCGSchedModel : SchedMachineModel {
  // Cycles for loads to access the cache [default = -1]
  let LoadLatency = 2;

  // Max micro-ops that can be buffered for optimized loop dispatch/execution.
  // [default = -1]
  let LoopMicroOpBufferSize = 0;

  // Allow scheduler to assign default model to any unrecognized opcodes.
  // [default = 1]
  let CompleteModel = 0;

  // Max micro-ops that may be scheduled per cycle. [default = 1]
  let IssueWidth = 1;

  // Extra cycles for a mispredicted branch. [default = -1]
  let MispredictPenalty = 10;

  // Enable Post RegAlloc Scheduler pass. [default = 0]
  let PostRAScheduler = 0;

  // Max micro-ops that can be buffered. [default = -1]
  let MicroOpBufferSize = 0;
}

let SchedModel = SampleCGSchedModel in {

def GenericIssueDIV : ProcResource<1> { }
def ALUDIV : SchedWriteRes<[GenericIssueDIV]> { let Latency = 15; }
def : ItinRW<[ALUDIV], [IIIdiv]>;

}

