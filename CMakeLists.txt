
set(LLVM_TARGET_DEFINITIONS SampleCG.td)

tablegen(LLVM SampleCGGenAsmMatcher.inc -gen-asm-matcher)
tablegen(LLVM SampleCGGenInstrInfo.inc -gen-instr-info)
tablegen(LLVM SampleCGGenAsmWriter.inc -gen-asm-writer)
tablegen(LLVM SampleCGGenRegisterInfo.inc -gen-register-info)
tablegen(LLVM SampleCGGenDAGISel.inc -gen-dag-isel)
tablegen(LLVM SampleCGGenCallingConv.inc -gen-callingconv)
tablegen(LLVM SampleCGGenSubtargetInfo.inc -gen-subtarget)
tablegen(LLVM SampleCGGenMCPseudoLowering.inc -gen-pseudo-lowering)
tablegen(LLVM SampleCGGenMCCodeEmitter.inc -gen-emitter)
tablegen(LLVM SampleCGGenDisassemblerTables.inc -gen-disassembler)


add_public_tablegen_target(SampleCGCommonTableGen)

add_llvm_target(SampleCGCodeGen
	SampleCGFrameLowering.cpp
	SampleCGInstrInfo.cpp
	SampleCGISelLowering.cpp
	SampleCGMachineFunction.cpp
	SampleCGRegisterInfo.cpp
	SampleCGSEFrameLowering.cpp
	SampleCGSEISelLowering.cpp
	SampleCGSERegisterInfo.cpp
	SampleCGSubtarget.cpp
	SampleCGTargetMachine.cpp
	SampleCGTargetObjectFile.cpp
	SampleCGSEInstrInfo.cpp
	SampleCGMCInstLower.cpp
	SampleCGAsmPrinter.cpp
	SampleCGISelDAGToDAG.cpp
	SampleCGSEISelDAGToDAG.cpp
	SampleCGAnalyzeImmediate.cpp
	SampleCGLongBranch.cpp
	SampleCGDelUselessJMP.cpp
	SampleCGDelaySlotFiller.cpp
	SampleCGEmitGPRestore.cpp
)

add_subdirectory(MCTargetDesc)
add_subdirectory(TargetInfo)
add_subdirectory(InstPrinter)
add_subdirectory(Disassembler)
add_subdirectory(AsmParser)

