program pretty_bench_example
    use pretty_bench, only: pretty_bench_init, pretty_bench_destroy, PrettyBench
    implicit none
    type(PrettyBench) :: pb

    pb = pretty_bench_init()

    start_bench("string")
    ! Do some stuff
    done_bench("string")


    call  output_benches()
    call pretty_bench_destroy(pb)
end program pretty_bench_example