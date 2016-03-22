#!/bin/bash

hask="./hask/Vec.lhs"
she="./she/Vec.lhs"
sing="./sing/Vec.lhs"

sources=("$she" "$sing" "$hask")

rm results.csv

touch results.csv

echo "Source,Lines,Characters" >> results.csv

for s in ${sources[@]}; do
    printf "%s" "$s," >> results.csv
    lines=`cat "$s" | wc -l`
    chars=`cat "$s" | wc -m`
    printf "%s" "$lines," >> results.csv
    echo "$chars" >> results.csv
done

exit 0
