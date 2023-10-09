#!/bin/zsh

splitLines() { echo (${(f)"$(echo $1)"}) }

file=$1
echo $file
lines=$(cat "${file}" | pcregrep -M "(# Properties)(\\n.+)+(\\n*#)" | pcregrep -M "\- .+:.+$")
kvs=$(echo $lines | sed "s/^[[:space:]]*-[[:space:]]//g" | sed -E 's/\[\[.+\|(.+)\]\]/\1/gm')
keys=$(echo $kvs | cut -d ":" -f 1 | sed -E 's/(\w+)( ([A-Za-z])(\w+))?/\L\1\E\U\3\E\4/gm')
values=$(echo $kvs | cut -d ":" -f 2)

echo "${keys}"
echo "${#keys[@]}"
for i in "${!keys[*]}"; do
    echo $i
    printf "${keys[i]}:: ${values[i]}\n"
done