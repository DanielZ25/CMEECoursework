#!/bin/bash
if [ $# -lt 3 ]; then
    echo -e "Error: Missing input file.\nUsage: bash ConcatenateTwoFiles.sh input1_file input2_file output_file"
    exit 1
fi

if [ ! -e "$1" ]; then
    echo -e "Error: Input file '$1' not found"
    exit 1
fi

if [ ! -e "$2" ]; then
    echo -e "Error: Input file '$2' not found"
    exit 1
fi

cat $1 > $3
cat $2 >> $3
echo "Merged File is"
cat "$3"
echo "\nConcatenation complete! Output saved as '$3'" 