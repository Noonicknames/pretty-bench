#!/bin/bash

source ./env.sh

mkdir out

gcc -std=c23 -O2 -c main.c -o out/main.o

gcc -std=c23 -O2 -L../../target/release -lpretty_bench -o out/pretty_bench_example \
    out/main.o