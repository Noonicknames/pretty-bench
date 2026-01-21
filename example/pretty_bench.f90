module pretty_bench
    use iso_c_binding, only: c_ptr
    implicit none
    private
    public new_pretty_bench, PrettyBench

    type, bind(C) :: PrettyBenchRaw
        type(c_ptr) :: inner
    end type PrettyBenchRaw
    
    type PrettyBench
        type(PrettyBenchRaw) :: inner
    contains
        procedure :: destroy, clone
    end type PrettyBench

    interface
        function pretty_bench_init() result(pb) bind(C, name = "pretty_bench_init")
            import :: PrettyBenchRaw
            type(PrettyBenchRaw) :: pb
        end function pretty_bench_init

        subroutine pretty_bench_destroy(pb) bind(C, name = "pretty_bench_destroy")
            import :: PrettyBenchRaw
            type(PrettyBenchRaw), intent(inout) :: pb
        end subroutine pretty_bench_destroy

        function pretty_bench_clone(pb) result(cloned_pb) bind(C, name = "pretty_bench_clone")
            import :: PrettyBenchRaw
            type(PrettyBenchRaw), intent(in) :: pb
            type(PrettyBenchRaw) :: cloned_pb
        end function 
    end interface
contains
    function new_pretty_bench() result(res)
        type(PrettyBench) res
        res%inner = pretty_bench_init()
    end function new_pretty_bench

    subroutine destroy(self)
        class(PrettyBench), intent(inout) :: self
        call pretty_bench_destroy(self%inner)
    end subroutine destroy
    
    function clone(self) result(res)
        class(PrettyBench), intent(in) :: self
        type(PrettyBench) :: res

        res%inner = pretty_bench_clone(self%inner)
    end function clone

end module pretty_bench