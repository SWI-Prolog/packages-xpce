cmake_minimum_required(VERSION 3.10)
project(win32-xpm)

add_library(libxpm STATIC
	    Attrib.c CrBufFrI.c CrDatFrI.c CrIFrBuf.c CrIFrDat.c
	    Image.c Info.c RdFToBuf.c RdFToDat.c RdFToI.c
	    WrFFrBuf.c WrFFrDat.c WrFFrI.c create.c
	    data.c hashtab.c misc.c parse.c rgb.c scan.c
	    simx.c)
target_compile_options(libxpm PRIVATE -DWIN32 -DFOR_MSW)
