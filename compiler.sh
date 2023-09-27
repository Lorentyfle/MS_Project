#!/bin/bash

# Set Fortran compiler
FC=gfortran
# Directory of the modules
MODULE_DIR="./modules"
SRC_DIR="./src"
NAME_PROG="PCCP_MC_LJ.x"
OUTPUT_DIR="./bin"
# Create the binary directory if it doesn't find it
mkdir -p $OUTPUT_DIR
# Compile the Fortran program
$FC $MODULE_DIR/*.f90 $SRC_DIR/*.f90 -o $OUTPUT_DIR/$NAME_PROG

if [ $? -eq 0 ]; then
    echo "Compilation successful."
else
    echo "Compilation failed."
fi

rm ./*.mod
