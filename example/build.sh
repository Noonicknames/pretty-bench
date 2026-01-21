#!/bin/bash

gfortran -c pretty_bench.f90 -o out/
gfortran -c main.f90 -o out/

gfortran -L../target/release -lpretty_bench -o pretty_bench_example \
    out/main.o out/pretty_bench.o