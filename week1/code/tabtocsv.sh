#!/bin/sh
# Author: Daniel Zhu haotian.zhu21@imperial.ac.uk
# Script: tabtocsv.sh
# Description: substitute the tabs in the files with commas
#
# Saves the output into a .csv file
# Arguments: 1 -> tab delimited file
# Date: Oct 2025

if [ $# -lt 1]; then
    echo -e "Error: Missing input file.\nUsage: bash tabtocsv.sh input_file"
    exit 1
fi

if [ ! -e "$1"]; then
    echo -e "Error: Input file '$1' not found"
    exit 1
fi

echo "Creating a comma delimited version of $1 ..."
cat $1 | tr -s "\t" "," >> $1.csv
echo "Done!"
exit