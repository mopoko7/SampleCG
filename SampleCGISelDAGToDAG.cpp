//===-- SampleCGISelDAGToDAG.cpp - A Dag to Dag Inst Selector for SampleCG --------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file defines an instruction selector for the SAMPLECG target.
//
//===----------------------------------------------------------------------===//

#include "SampleCGISelDAGToDAG.h"
#include "MCTargetDesc/SampleCGBaseInfo.h"
#include "SampleCG.h"
#include "SampleCGMachineFunction.h"
#include "SampleCGRegisterInfo.h"
#include "llvm/CodeGen/MachineConstantPool.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/SelectionDAGNodes.h"
#include "llvm/CodeGen/StackProtector.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/Type.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
using namespace llvm;

#define DEBUG_TYPE "samplecg-isel"

//===----------------------------------------------------------------------===//
// Instruction Selector Implementation
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// SampleCGDAGToDAGISel - SAMPLECG specific code to select SAMPLECG machine
// instructions for SelectionDAG operations.
//===----------------------------------------------------------------------===//

void SampleCGDAGToDAGISel::getAnalysisUsage(AnalysisUsage &AU) const {
  // There are multiple SampleCGDAGToDAGISel instances added to the pass pipeline.
  // We need to preserve StackProtector for the next one.
  AU.addPreserved<StackProtector>();
  SelectionDAGISel::getAnalysisUsage(AU);
}

bool SampleCGDAGToDAGISel::runOnMachineFunction(MachineFunction &MF) {
  Subtarget = &static_cast<const SampleCGSubtarget &>(MF.getSubtarget());
  bool Ret = SelectionDAGISel::runOnMachineFunction(MF);

  processFunctionAfterISel(MF);

  return Ret;
}

bool SampleCGDAGToDAGISel::SelectAddr(SDNode *Parent, SDValue Addr, SDValue &Base,
                                        SDValue &Offset) {
	EVT ValTy = Addr.getValueType();
	SDLoc DL(Addr);

	 // If Parent is an unaligned f32 load or store, select a (base + index)
  // floating point load/store instruction (luxc1 or suxc1).
  const LSBaseSDNode* LS = 0;

  if (Parent && (LS = dyn_cast<LSBaseSDNode>(Parent))) {
    EVT VT = LS->getMemoryVT();

    if (VT.getSizeInBits() / 8 > LS->getAlignment()) {
      assert("Unaligned loads/stores not supported for this type.");
      if (VT == MVT::f32)
        return false;
    }
  }

  // if Address is FI, get the TargetFrameIndex.
  if (FrameIndexSDNode *FIN = dyn_cast<FrameIndexSDNode>(Addr)) {
    Base   = CurDAG->getTargetFrameIndex(FIN->getIndex(), ValTy);
    Offset = CurDAG->getTargetConstant(0, DL, ValTy);
    return true;
  }

  // on PIC code Load GA
  if (Addr.getOpcode() == SampleCGISD::Wrapper) {
    Base   = Addr.getOperand(0);
    Offset = Addr.getOperand(1);
    return true;
  }

  //@static
  if (TM.getRelocationModel() != Reloc::PIC_) {
    if ((Addr.getOpcode() == ISD::TargetExternalSymbol ||
        Addr.getOpcode() == ISD::TargetGlobalAddress))
      return false;
  }

    // Addresses of the form FI+const or FI|const
  if (CurDAG->isBaseWithConstantOffset(Addr)) {
    ConstantSDNode *CN = dyn_cast<ConstantSDNode>(Addr.getOperand(1));
    if (isInt<16>(CN->getSExtValue())) {

      // If the first operand is a FI, get the TargetFI Node
      if (FrameIndexSDNode *FIN = dyn_cast<FrameIndexSDNode>
                                  (Addr.getOperand(0)))
        Base = CurDAG->getTargetFrameIndex(FIN->getIndex(), ValTy);
      else
        Base = Addr.getOperand(0);

      Offset = CurDAG->getTargetConstant(CN->getZExtValue(), DL, ValTy);
      return true;
    }
  }

  Base   = Addr;
  Offset = CurDAG->getTargetConstant(0, DL, ValTy);
  return true;
}

/// Select instructions not customized! Used for
/// expanded, promoted and normal instructions
void SampleCGDAGToDAGISel::Select(SDNode *Node) {
  unsigned Opcode = Node->getOpcode();

	LLVM_DEBUG(errs() << "Selecting: "; Node->dump(CurDAG); errs() << "\n");

  // If we have a custom node, we already have selected!
  if (Node->isMachineOpcode()) {
    LLVM_DEBUG(errs() << "== "; Node->dump(CurDAG); errs() << "\n");
    Node->setNodeId(-1);
    return;
  }

  // See if subclasses can handle this node.
  if (trySelect(Node))
    return;

  switch(Opcode) {
  case ISD::GLOBAL_OFFSET_TABLE:
	  ReplaceNode(Node, getGlobalBaseReg());
	  return;
  default: break;
  }

  // Select the default instruction
  SelectCode(Node);
}

bool SampleCGDAGToDAGISel::
SelectInlineAsmMemoryOperand(const SDValue &Op, unsigned ConstraintID,
                             std::vector<SDValue> &OutOps) {
  // All memory constraints can at least accept raw pointers.
  switch(ConstraintID) {
  default:
    llvm_unreachable("Unexpected asm memory constraint");
  case InlineAsm::Constraint_i:
  case InlineAsm::Constraint_m:
  case InlineAsm::Constraint_R:
  case InlineAsm::Constraint_ZC:
    OutOps.push_back(Op);
    return false;
  }
  return true;
}

SDNode*SampleCGDAGToDAGISel::getGlobalBaseReg() {
	unsigned GlobalBaseReg = MF->getInfo<SampleCGFunctionInfo>()->getGlobalBaseReg();
	return CurDAG->getRegister(GlobalBaseReg, getTargetLowering()->getPointerTy(
				CurDAG->getDataLayout())).getNode();
}

