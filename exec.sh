#!/bin/bash

for file in *.conllu; do
    sed -i '/^#/d' "$file"
    echo "Processed $file"
done
