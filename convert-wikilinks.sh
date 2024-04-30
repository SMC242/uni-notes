#!/bin/bash

# Regular expression to match Obsidian-style wikilinks
pattern='\[\[([^|\]]+)(\|([^|\]]+))?\]\]'

# Function to replace wikilinks in a given line
replace_wikilinks() {
    local line="$1"

    echo "line is $line"
    while [[ $line =~ $pattern ]]; do
        link="${BASH_REMATCH[1]}"
        alias="${BASH_REMATCH[3]}"
        echo "Link: $link"
        # Check if there is an alias
        if [[ -n $alias ]]; then
            # Replace the wikilink with the alias
            line="${line/${BASH_REMATCH[0]}/$alias}"
        else
            # If no alias, extract target file name
            target="${link##*/}"
            # Replace underscores with spaces
            target=$(echo "$target" | sed 's/_/ /g')
            # Replace the wikilink with the target file name
            line="${line/${BASH_REMATCH[0]}/$target}"
        fi
    done
    echo "$line"
}

# Iterate over lines of input
while IFS= read -r line; do
    # Replace wikilinks in the line
    line=$(replace_wikilinks "$line")
    # Output the modified line
    echo "$line"
done
