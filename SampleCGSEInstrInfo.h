//===-- SampleCGSEInstrInfo.h - SampleCG32/64 Instruction Information ---*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the SampleCG32/64 implementation of the TargetInstrInfo class.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_SAMPLECG_SAMPLECGSEINSTRINFO_H
#define LLVM_LIB_TARGET_SAMPLECG_SAMPLECGSEINSTRINFO_H

#include "SampleCGInstrInfo.h"
#include "SampleCGSERegisterInfo.h"

namespace llvm {

class SampleCGSEInstrInfo : public SampleCGInstrInfo {
  const SampleCGSERegisterInfo RI;
  const SampleCGRegisterInfo &getRegisterInfo() const override;

private:
  void expandRetLR(MachineBasicBlock &MBB, MachineBasicBlock::iterator I) const;

  unsigned getOppositeBranchOpc(unsigned Opc) const override;

  void expandEhReturn(MachineBasicBlock &MBB,
    MachineBasicBlock::iterator I) const;


public:
  explicit SampleCGSEInstrInfo(const SampleCGSubtarget &STI);

  bool expandPostRAPseudo(MachineInstr &MI) const override;

  void adjustStackPtr(unsigned SP, int64_t Amount, MachineBasicBlock &MBB,
		  				MachineBasicBlock::iterator I) const override;

  unsigned loadImmediate(int64_t Imm, MachineBasicBlock &MBB,
		  					MachineBasicBlock::iterator II, const DebugLoc &DL,
							unsigned *NewImm) const;

	void storeRegToStack(MachineBasicBlock &MBB,
							MachineBasicBlock::iterator MI,
							unsigned SrcReg,
							bool isKill,
							int FrameIndex,
							const TargetRegisterClass*RC,
							const TargetRegisterInfo*TRI,
							int64_t Offset) const override;

	void loadRegFromStack(MachineBasicBlock &MBB,
							MachineBasicBlock::iterator MI,
							unsigned DestReg,
							int FrameIndex,
							const TargetRegisterClass*RC,
							const TargetRegisterInfo *TRI,
							int64_t Offset) const override;
	void copyPhysReg(MachineBasicBlock &MBB, MachineBasicBlock::iterator MI,
							const DebugLoc &DL,
							unsigned DestReg,
							unsigned SrcReg,
							bool KillSrc) const override;

};

}

#endif
