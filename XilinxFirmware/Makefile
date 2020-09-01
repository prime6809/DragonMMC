#
# Makefile for DragonMMC xlinx firmware.

VPATH      = $(ROOT)/source

XILINX      ?= /cygdrive/d/Xilinx/14.7

PATH       := $(PATH):${XILINX}/ISE_DS/ISE/bin/nt64:${PAPILIO}/linux64
SHELL      := /bin/bash

SRCPATH		:= ./source
all: firmware


firmware: 
		xtclsh ise_build.tcl DragonMMC.xise
		