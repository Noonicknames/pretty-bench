module pretty_bench
    use iso_c_binding, only: c_ptr
    implicit none

    

    type, bind(C) :: PrettyBench
        type(c_ptr) :: inner
    contains
        procedure :: destroy
        procedure :: clone
    end type PrettyBench

    interface
        function pretty_bench_init() result(pb) bind(C, name = "pretty_bench_init")
            import :: PrettyBench
            type(PrettyBench) :: pb
        end function pretty_bench_init

        subroutine pretty_bench_destroy(pb) bind(C, name = "pretty_bench_destroy")
            import :: PrettyBench
            type(PrettyBench), intent(inout) :: pb
        end subroutine pretty_bench_destroy

        function pretty_bench_clone(pb) result(cloned_pb) bind(C, nmae = "pretty_bench_clone")
            import :: PrettyBench
            type(PrettyBench), intent(in) :: pb
            type(PrettyBench) :: cloned_pb
        end function 
    end interface
contains
    subroutine destroy(self)
        class(PrettyBench), intent(inout) :: self
        call pretty_bench_destroy(self)
    end subroutine destroy
    
    function clone(self) result(res)
        class(PrettyBench), intent(in) :: self
        type(PrettyBench) :: res

        res = pretty_bench_clone(self)
    end function clone

end module pretty_bench