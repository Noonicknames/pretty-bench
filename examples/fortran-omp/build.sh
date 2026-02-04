#!/bin/bash

source ./env.sh

mkdir out

compiler=gfortran
flags=(-O3 -fopenmp -L../../target/release -lpretty_bench)

${compiler} ${flags[@]} -c ../../bindings/headers/pretty_bench.f90 -o out/pretty_bench.o
${compiler} ${flags[@]} -c main.f90 -o out/main.o

${compiler} ${flags[@]} -o out/pretty_bench_example \
    out/main.o out/pretty_bench.o