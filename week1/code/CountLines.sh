#!/bin/bash

if [ $# -lt 1 ]; then
    echo "Error: Missing input file."
    echo "Usage: bash CountLines.sh input_file"
    exit 1
fi

# Check if input file exists
if [ ! -e "$1" ]; then
    echo "Error: Input file '$1' not found"
    exit 1
fi


NumLines=`wc -l < $1`
echo "The file $1 has $NumLines lines"
echo