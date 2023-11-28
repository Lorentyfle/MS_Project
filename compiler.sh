#!/bin/bash

# Set Fortran compiler
FC=gfortran
# Directory of the modules
MODULE_DIR="./modules"
SRC_DIR="./src"
NAME_PROG="PCCP_MC_LJ.x"
OUTPUT_DIR="./bin"
IO_DIR="./Input_output"
RDF_O="./Input_output/RDF_output"
# Create the binary directory if it doesn't find it
mkdir -p $OUTPUT_DIR
# Create the input_output directory if it doesn't find it
mkdir -p $IO_DIR
mkdir -p $RDF_O
# Compile the Fortran program
$FC $MODULE_DIR/*.f90 $SRC_DIR/*.f90 -o $OUTPUT_DIR/$NAME_PROG

if [ $? -eq 0 ]; then
    echo "Compilation successful."
else
    echo "Compilation failed."
fi

rm ./*.mod
