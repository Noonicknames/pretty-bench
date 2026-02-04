program pretty_bench_example
    use iso_fortran_env, only: int64
    use iso_c_binding, only: c_long_long
    use pretty_bench, only: t_pretty_bench, pb_new, t_arc_str, arc_str, ffi_str, pb_sleep, duration_micros, duration_nanos, t_duration, operator (+), operator (-)
    use omp_lib
    implicit none
    type(t_pretty_bench) :: pb
    type(t_arc_str) :: bench_group_name

    write(*, "(A,i4)") "OpenMP Max threads: ", omp_get_max_threads()

    ! Creation of an t_arc_str which can be efficiently copied.
    bench_group_name = arc_str("Diagonalisation of a 0x0 matrix")

    ! An t_arc_str can be converted to a character, dimension(:) and printed.
    print *, bench_group_name%as_f_str()

    ! Create t_pretty_bench struct
    pb = pb_new()
    
    ! Ensure to clone since we still want to keep a copy of the group name ourselves.
    call pb%new_group(bench_group_name%clone(), duration_nanos(10_int64))

    ! Import previous benchmarks
    call pb%import_from_file(ffi_str("test.bench"))

    ! Example benchmark
    block
        type(t_duration) :: bench_id, sleep_duration
        integer :: i

        sleep_duration = duration_micros(100_int64)
        !$OMP PARALLEL SHARED(sleep_duration) PRIVATE(bench_id, i)
            !$OMP DO SCHEDULE(DYNAMIC, 1)
            do i = 1, 1000
                bench_id = pb%start_bench()
                call pb_sleep(sleep_duration)
                call pb%end_bench(ffi_str(bench_group_name), bench_id)
            end do
            !$OMP END DO
        !$OMP END PARALLEL
    end block

    ! You can drop the t_arc_str now, this will not delete pb's copy.
    call bench_group_name%drop()

    ! Output to file
    call pb%serialise_to_file(ffi_str("test.bench"))

    call pb%print_histograms()

    ! Clean up resources
    call pb%drop()
end program pretty_bench_example