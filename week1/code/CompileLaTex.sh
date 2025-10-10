#!/bin/bash

if [ $# -lt 1 ]; then
    echo "Usage: bash CompileLaTex.sh <filename.tex>"
    exit 1
fi


pdflatex $1
bibtex ${1%.tex}
pdflatex $1
pdflatex $1
evince ${1%.tex}.pdf &

## Cleanup
rm *.aux
rm *.log
rm *.bbl
rm *.blg