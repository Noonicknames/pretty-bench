#!/bin/bash

mkdir out

gfortran -O2 -c pretty_bench.f90 -o out/pretty_bench.o
gfortran -O2 -c main.f90 -o out/main.o

gfortran -O2 -L../target/release -lpretty_bench -o pretty_bench_example \
    out/main.o out/pretty_bench.o