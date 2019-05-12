//===-- SampleCGBaseInfo.h - Top level definitions for SampleCG MC ----*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains small standalone helper functions and enum definitions for
// the SampleCG target useful for the compiler back-end and the MC libraries.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_SAMPLECG_MCTARGETDESC_SAMPLECGBASEINFO_H
#define LLVM_LIB_TARGET_SAMPLECG_MCTARGETDESC_SAMPLECGBASEINFO_H

#include "SampleCGFixupKinds.h"
#include "SampleCGMCTargetDesc.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/Support/DataTypes.h"
#include "llvm/Support/ErrorHandling.h"

namespace llvm {

// SampleCGII - This namespace holds all of the target specific flags that
// instruction info tracks.
namespace SampleCGII {
  // Target Operand Flag enum.
  enum TOF {
    //===------------------------------------------------------------------===//
    // Cpu0 Specific MachineOperand flags.

    MO_NO_FLAG,

    /// MO_GOT - Represents the offset into the global offset table at which
    /// the address the relocation entry symbol resides during execution.
    MO_GOT,

    /// MO_GOT_CALL - Represents the offset into the global offset table at
    /// which the address of a call site relocation entry symbol resides
    /// during execution. This is different from the above since this flag
    /// can only be present in call instructions.
    MO_GOT_CALL,

    /// MO_GPREL - Represents the offset from the current gp value to be used
    /// for the relocatable object file being produced.
    MO_GPREL,

    /// MO_ABS_HI/LO - Represents the hi or low part of an absolute symbol
    /// address.
    MO_ABS_HI,
    MO_ABS_LO,

    /// MO_TLSGD - Represents the offset into the global offset table at which
    // the module ID and TSL block offset reside during execution (General
    // Dynamic TLS).
    MO_TLSGD,

    /// MO_TLSLDM - Represents the offset into the global offset table at which
    // the module ID and TSL block offset reside during execution (Local
    // Dynamic TLS).
    MO_TLSLDM,
    MO_DTP_HI,
    MO_DTP_LO,

    /// MO_GOTTPREL - Represents the offset from the thread pointer (Initial
    // Exec TLS).
    MO_GOTTPREL,

    /// MO_TPREL_HI/LO - Represents the hi and low part of the offset from
    // the thread pointer (Local Exec TLS).
    MO_TP_HI,
    MO_TP_LO,

    /// MO_GOT_HI16/LO16 - Relocations used for large GOTs.
    MO_GOT_HI16,
    MO_GOT_LO16
  }; // enum TOF {
enum {
	Pseudo   = 0,
	FrmR  = 1,
	FrmI  = 2,
	FrmJ  = 3,
	FrmOther = 4,
	FormMask = 15
};
} // namespace SampleCGII

} // namespace llvm
#endif // LLVM_LIB_TARGET_SAMPLECG_MCTARGETDESC_SAMPLECGBASEINFO_H
