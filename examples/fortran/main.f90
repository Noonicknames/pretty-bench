program pretty_bench_example
    use iso_fortran_env, only: int64
    use iso_c_binding, only: c_long_long
    use pretty_bench, only: PrettyBench, pb_new, ArcStr, arc_str, ffi_str, pb_sleep
    implicit none
    type(PrettyBench) :: pb
    type(ArcStr) :: bench_group_name

    ! Creation of an ArcStr which can be efficiently copied.
    bench_group_name = arc_str("Diagonalisation of a 0x0 matrix")

    ! An ArcStr can be converted to a character, dimension(:) and printed.
    print *, bench_group_name%as_f_str()

    ! Create PrettyBench struct
    pb = pb_new()
    
    ! Ensure to clone since we still want to keep a copy of the group name ourselves.
    call pb%new_group(bench_group_name%clone())

    ! Import previous benchmarks
    call pb%import_from_file(ffi_str("test.bench"))

    ! Example benchmark
    block
        integer(c_long_long) :: bench_id
        integer :: i
        do i = 1, 1000
            bench_id = pb%start_bench(ffi_str(bench_group_name))
            call pb_sleep(100000_int64)
            call pb%end_bench(ffi_str(bench_group_name), bench_id)
        end do
    end block

    ! You can drop the ArcStr now, this will not delete pb's copy.
    call bench_group_name%drop()

    ! Output to file
    call pb%serialise_to_file(ffi_str("test.bench"))

    call pb%print_histograms()

    ! Clean up resources
    call pb%drop()
end program pretty_bench_example