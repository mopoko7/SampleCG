//===-- SampleCGMCTargetDesc.h - SampleCG Target Descriptions ---------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file provides SampleCG specific target descriptions.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_SAMPLECG_MCTARGETDESC_SAMPLECGMCTARGETDESC_H
#define LLVM_LIB_TARGET_SAMPLECG_MCTARGETDESC_SAMPLECGMCTARGETDESC_H

#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCTargetOptions.h"
#include "llvm/Support/DataTypes.h"
#include "llvm/MC/MCObjectWriter.h"

namespace llvm {
class MCAsmBackend;
class MCCodeEmitter;
class MCContext;
class MCInstrInfo;
class MCInstrAnalysis;
class MCObjectTargetWriter;
class MCRelocationInfo;
class MCSubtargetInfo;
class Target;
class Triple;
class StringRef;
class raw_pwrite_stream;

Target &getTheSampleCGTarget();
Target &getTheSampleCGelTarget();

MCCodeEmitter *createSampleCGMCCodeEmitter(const MCInstrInfo &MCII,
                                        const MCRegisterInfo &MRI,
                                        MCContext &Ctx);

MCAsmBackend *createSampleCGAsmBackend(const Target &T, const MCSubtargetInfo &STI,
                                    const MCRegisterInfo &MRI,
                                    const MCTargetOptions &Options);

MCCodeEmitter *createSampleCGMCCodeEmitterEB(const MCInstrInfo &MCII,
												const MCRegisterInfo &MRI,
												MCContext &Ctx);

MCCodeEmitter *createSampleCGMCCodeEmitterEL(const MCInstrInfo &MCII,
												const MCRegisterInfo &MRI,
												MCContext &Ctx);

/*
MCAsmBackend *createSampleCGAsmBackendEB32(const Target &T,
											const MCRegisterInfo &MRI,
											const Triple &TT,
											StringRef CPU);

MCAsmBackend *createSampleCGAsmBackendEL32(const Target &T,
											const MCRegisterInfo &MRI,
											const Triple &TT,
											StringRef CPU);
*/

MCAsmBackend *createSampleCGAsmBackendEL32(const Target &T,
                                         const MCSubtargetInfo &STI,
                                         const MCRegisterInfo &MRI,
                                         const MCTargetOptions &Options);

MCAsmBackend *createSampleCGAsmBackendEB32(const Target &T,
                                         const MCSubtargetInfo &STI,
                                         const MCRegisterInfo &MRI,
                                         const MCTargetOptions &Options);

MCObjectWriter *createSampleCGELFObjectWriter(raw_pwrite_stream &OS,
												uint8_t OSABI,
												bool IsLittleEndian);

std::unique_ptr<MCObjectTargetWriter> createSampleCGELFObjectWriter(uint8_t OSABI);
} // namespace llvm

// Defines symbolic names for SampleCG registers.  This defines a mapping from
// register name to register number.
#define GET_REGINFO_ENUM
#include "SampleCGGenRegisterInfo.inc"

// Defines symbolic names for the SampleCG instructions.
#define GET_INSTRINFO_ENUM
#include "SampleCGGenInstrInfo.inc"

#define GET_SUBTARGETINFO_ENUM
#include "SampleCGGenSubtargetInfo.inc"

#endif // LLVM_LIB_TARGET_SAMPLECG_MCTARGETDESC_SAMPLECGMCTARGETDESC_H
