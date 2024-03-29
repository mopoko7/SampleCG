//===-- SampleCGMCAsmInfo.cpp - SampleCG asm properties -----------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the declarations of the SampleCGMCAsmInfo properties.
//
//===----------------------------------------------------------------------===//

#include "SampleCGMCAsmInfo.h"

#include "llvm/ADT/Triple.h"

using namespace llvm;

void SampleCGMCAsmInfo::anchor() {}

SampleCGMCAsmInfo::SampleCGMCAsmInfo(const Triple & /*TheTriple*/) {
  IsLittleEndian = false;
  PrivateGlobalPrefix = ".L";
  WeakRefDirective = "\t.weak\t";
  ExceptionsType = ExceptionHandling::DwarfCFI;

  // SampleCG assembly requires ".section" before ".bss"
  UsesELFSectionDirectiveForBSS = true;

  // Use the integrated assembler instead of system one.
  UseIntegratedAssembler = true;

  // Use '!' as comment string to correspond with old toolchain.
  CommentString = "!";

  // Target supports emission of debugging information.
  SupportsDebugInformation = true;

  // Set the instruction alignment. Currently used only for address adjustment
  // in dwarf generation.
  MinInstAlignment = 4;
}
