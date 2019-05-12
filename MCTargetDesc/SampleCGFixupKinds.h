//===-- SampleCGFixupKinds.h - SampleCG Specific Fixup Entries ----------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_SAMPLECG_MCTARGETDESC_SAMPLECGFIXUPKINDS_H
#define LLVM_LIB_TARGET_SAMPLECG_MCTARGETDESC_SAMPLECGFIXUPKINDS_H

#include "llvm/MC/MCFixup.h"

namespace llvm {
namespace SampleCG {
  // Although most of the current fixup types reflect a unique relocation
  // one can have multiple fixup types for a given relocation and thus need
  // to be uniquely named.
  //
  // This table *must* be in the same order of
  // MCFixupKindInfo Infos[SampleCG::NumTargetFixupKinds]
  // in SampleCGAsmBackend.cpp.
  //
  enum Fixups {
    //@ Pure upper 32 bit fixup resulting in - R_CPU0_32.
    fixup_SampleCG_32 = FirstTargetFixupKind,

    // Pure upper 16 bit fixup resulting in - R_CPU0_HI16.
    fixup_SampleCG_HI16,

    // Pure lower 16 bit fixup resulting in - R_CPU0_LO16.
    fixup_SampleCG_LO16,

    // 16 bit fixup for GP offest resulting in - R_CPU0_GPREL16.
    fixup_SampleCG_GPREL16,

    // Symbol fixup resulting in - R_CPU0_GOT16.
    fixup_SampleCG_GOT,

    // PC relative branch fixup resulting in - R_CPU0_PC16.
    // cpu0 PC16, e.g. beq
    fixup_SampleCG_PC16,

    // PC relative branch fixup resulting in - R_CPU0_PC24.
    // cpu0 PC24, e.g. jeq, jmp
    fixup_SampleCG_PC24,
    
    // resulting in - R_CPU0_CALL16.
    fixup_SampleCG_CALL16,

    // resulting in - R_CPU0_TLS_GD.
    fixup_SampleCG_TLSGD,

    // resulting in - R_CPU0_TLS_GOTTPREL.
    fixup_SampleCG_GOTTPREL,

    // resulting in - R_CPU0_TLS_TPREL_HI16.
    fixup_SampleCG_TP_HI,

    // resulting in - R_CPU0_TLS_TPREL_LO16.
    fixup_SampleCG_TP_LO,

    // resulting in - R_CPU0_TLS_LDM.
    fixup_SampleCG_TLSLDM,

    // resulting in - R_CPU0_TLS_DTP_HI16.
    fixup_SampleCG_DTP_HI,

    // resulting in - R_CPU0_TLS_DTP_LO16.
    fixup_SampleCG_DTP_LO,

    // resulting in - R_CPU0_GOT_HI16
    fixup_SampleCG_GOT_HI16,

    // resulting in - R_CPU0_GOT_LO16
    fixup_SampleCG_GOT_LO16,

    // Marker
    LastTargetFixupKind,
    NumTargetFixupKinds = LastTargetFixupKind - FirstTargetFixupKind
  };
  //@Fixups }
} // namespace SampleCG
} // namespace llvm


#endif
