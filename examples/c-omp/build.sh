#!/bin/bash

source ./env.sh

mkdir out

compiler=gcc
flags=(-std=c23 -O3  -L../../target/release -lpretty_bench -fopenmp)

${compiler} ${flags[@]} -c main.c -o out/main.o

${compiler} ${flags[@]} -o out/pretty_bench_example \
    out/main.o