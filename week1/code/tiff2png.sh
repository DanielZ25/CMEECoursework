#!/bin/bash

# need at least one input file
[ $# -eq 0 ] && echo -e "No tif file provided. \nUsage: bash tiff2png.sh *.tif" && exit 1


# convert each input file
for f in "$@"; do
    [ -f "$f" ] || { echo "Cannot find $f"; continue; }
    out="../results/$(basename "$f" .tif).png"
    echo "Converting $f"
    convert "$f" "$out"
done
