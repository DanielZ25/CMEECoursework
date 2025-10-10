#!/bin/bash

if [ $# -lt 2 ]; then
    echo -e "Error: Missing input or output files\nUsage: bash csvtospace.sh input.csv output.csv"
    exit 1
fi 

if [ ! -e "$1" ]; then
    echo -e "Error: No such file or directory\nInput file not found"
    exit 1
fi


cat $1 | tr ',' ' ' > $2
echo "The revised csv is $2"
cat $2

 echo -e "Conversion complete!\nOutput saved as: $2"