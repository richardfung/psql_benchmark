#!/bin/bash

source init.sh
query_types=(simple complex small big)

for t in `seq 1 $2`;
do
    for query_type in ${query_types[@]};
    do
        for i in `seq 1 $1`;
        do
            filename="results/go/"
            filename+="$query_type"
            filename+="_t$t"
            go run bench.go -q 1000 -t $t -qt $query_type >> $filename

            filename="results/python/"
            filename+="$query_type"
            filename+="_t$t"
            python bench.py -q 1000 -t $t -qt $query_type >> $filename
        done
    done
done
