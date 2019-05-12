//===-- SampleCGCallingConv.td - Calling Conventions for SampleCG --*- tablegen -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
// This describes the calling conventions for SampleCG architecture.
//===----------------------------------------------------------------------===//

/// CCIfSubtarget - Match if the current subtarget has a feature F.
class CCIfSubtarget<string F, CCAction A, string Invert = "">
    : CCIf<!strconcat(Invert, "static_cast<const SampleCGSubtarget&>"
			"(State.getMachineFunction().getSubtarget()).", F), A>;

def CSR_O32 : CalleeSavedRegs<(add LR, FP, V0,
                                   (sequence "S%u", 1, 0))>;

def RetCC_SampleCGEABI : CallingConv<[
	// i32 are returnedinregisters V0, V1, A0, A1
	CCIfType<[i32], CCAssignToReg<[V0, V1, A0, A1]>>
]>;

def RetCC_SampleCG : CallingConv<[
	CCDelegateTo<RetCC_SampleCGEABI>
]>;
