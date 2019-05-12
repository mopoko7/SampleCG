//===---- SampleCGABIInfo.h - Information about SAMPLECG ABI's --------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_SAMPLECG_MCTARGETDESC_SAMPLECGABIINFO_H
#define LLVM_LIB_TARGET_SAMPLECG_MCTARGETDESC_SAMPLECGABIINFO_H

#include "llvm/ADT/Triple.h"
#include "llvm/IR/CallingConv.h"
#include "llvm/MC/MCRegisterInfo.h"

namespace llvm {

template <typename T> class ArrayRef;
class MCTargetOptions;
class StringRef;
class TargetRegisterClass;

class SampleCGABIInfo {
public:
  enum class ABI { Unknown, O32, S32 };

protected:
  ABI ThisABI;

public:
  SampleCGABIInfo(ABI ThisABI) : ThisABI(ThisABI) {}

  static SampleCGABIInfo Unknown() { return SampleCGABIInfo(ABI::Unknown); }
  static SampleCGABIInfo O32() { return SampleCGABIInfo(ABI::O32); }
  static SampleCGABIInfo S32() { return SampleCGABIInfo(ABI::S32); }
  static SampleCGABIInfo computeTargetABI(const Triple &TT, StringRef CPU,
                                      const MCTargetOptions &Options);

  bool IsKnown() const { return ThisABI != ABI::Unknown; }
  bool IsO32() const { return ThisABI == ABI::O32; }
  bool IsS32() const { return ThisABI == ABI::S32; }
  ABI GetEnumValue() const { return ThisABI; }

  /// The registers to use for byval arguments.
  ArrayRef<MCPhysReg> GetByValArgRegs() const;

  /// The registers to use for the variable argument list.
  ArrayRef<MCPhysReg> GetVarArgRegs() const;

  /// Obtain the size of the area allocated by the callee for arguments.
  /// CallingConv::FastCall affects the value for O32.
  unsigned GetCalleeAllocdArgSizeInBytes(CallingConv::ID CC) const;

  /// Ordering of ABI's
  /// SampleCGGenSubtargetInfo.inc will use this to resolve conflicts when given
  /// multiple ABI options.
  bool operator<(const SampleCGABIInfo Other) const {
    return ThisABI < Other.GetEnumValue();
  }

  unsigned GetStackPtr() const;
  unsigned GetFramePtr() const;
  unsigned GetNullPtr() const;
  unsigned GetEhDataReg(unsigned I) const;
  int EhDataRegSize() const;
};
}

#endif
