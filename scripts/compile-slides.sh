#!/bin/bash

FILE=$1
no_extension="${FILE%.*}"
OUTPUT_PATH=$2
OUTPUT_PATH=${OUTPUT_PATH:="${no_extension}.html"}
echo $OUTPUT_PATH
pandoc -t revealjs -o "${OUTPUT_PATH}" -s "${FILE}" 
