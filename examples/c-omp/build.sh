#!/bin/bash

source ./env.sh

mkdir out

flags=(-std=c23 -O2  -L../../target/release -lpretty_bench -fopenmp)

gcc ${flags[@]} -c main.c -o out/main.o

gcc ${flags[@]} -o out/pretty_bench_example \
    out/main.o