#!/bin/bash

hask="./hask/Vec.hs"
she="./she/Vec.hs"
sing="./sing/Vec.hs"
agda="./agda/Vec.agda"

sources=("$she" "$sing" "$hask" "$agda")
names=("SHE" "Singleton\nLibrary" "Haskell" "Agda")

rm results.csv

touch results.csv

echo "# Source,Lines,Characters" >> results.csv

for ((i=0;i<${#sources[@]};++i)); do
    printf "%s" "${names[i]}," >> results.csv
    lines=`cat "${sources[i]}" | wc -l`
    chars=`cat "${sources[i]}" | wc -m`
    printf "%s" "$lines," >> results.csv
    echo "$chars" >> results.csv
done

exit 0

${array[i]}
