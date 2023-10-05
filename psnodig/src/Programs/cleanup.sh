#!/bin/bash

extensions=("aux" "fdb_latexmk" "fls" "log" "synctex.gz")

condition=""
for ext in "${extensions[@]}"; do
    if [ -z "$condition" ]; then
        condition="-name \"*.$ext\""
    else
        condition="$condition -o -name \"*.$ext\""
    fi
done

eval "find . -type f \( $condition \) -delete"

# Legg ting i underfolder istedenfor å slette