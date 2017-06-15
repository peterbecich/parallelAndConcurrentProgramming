# https://wiki.haskell.org/FFI_Introduction

trim:
	cc -c -o trim.o trim.c

FFI_Example: trim.o
	ghc --make -main-is Chapter15DebuggingTuningFFI.ForeignFunctionInterface -o ffi_example ForeignFunctionInterface.hs trim.o
