program pretty_bench_example
    use pretty_bench, only: PrettyBench, new_pretty_bench
    implicit none
    type(PrettyBench) :: pb

    pb = new_pretty_bench()
    call pb%destroy()
end program pretty_bench_example