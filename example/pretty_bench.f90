module pretty_bench
    use iso_c_binding, only: c_ptr, c_long_long, c_loc, c_f_pointer, c_null_ptr
    implicit none
    private
    public pb_sleep, pb_new, PrettyBench, arc_str, ArcStr, ffi_str

    type, bind(C) :: FFIStrRaw
        type(c_ptr) :: ptr = c_null_ptr
        integer(c_long_long) :: len = 0
    end type FFIStrRaw

    type, bind(C) :: ArcFFIStrRaw
        type(c_ptr) :: ptr = c_null_ptr
        integer(c_long_long) :: len = 0
    end type ArcFFIStrRaw

    type FFIStr
        type(FFIStrRaw) :: inner
    contains
        procedure :: as_f_str => ffi_str_as_f_str
    end type FFIStr

    type ArcStr
        type(ArcFFIStrRaw) :: inner
    contains
        procedure :: clone => arc_str_clone, &
                     drop => arc_str_drop, &
                     as_f_str => arc_str_as_f_str
    end type ArcStr

    type, bind(C) :: PrettyBenchRaw
        type(c_ptr) :: inner
    end type PrettyBenchRaw
    
    type PrettyBench
        type(PrettyBenchRaw) :: inner
    contains
        procedure :: clone => pb_clone, &
                     drop => pb_drop, &
                     new_group => pb_new_group, &
                     start_bench => pb_start_bench, &
                     end_bench => pb_end_bench, &
                     serialise_to_file => pb_serialise_to_file, &
                     serialise_append_to_file => pb_serialise_append_to_file, &
                     import_from_file => pb_import_from_file, &
                     print_histograms => pb_print_histograms
    end type PrettyBench

    interface arc_str
        module procedure arc_str_scalar
        module procedure arc_str_array
        module procedure arc_str_arc_str
        module procedure arc_str_ffi_str
    end interface

    interface ffi_str
        module procedure ffi_str_scalar
        module procedure ffi_str_array
        module procedure ffi_str_arc_str
        module procedure ffi_str_ffi_str
    end interface

    interface
        subroutine pb_sleep_(nanos) bind(C, name = "pb_sleep")
            use iso_c_binding, only: c_long_long
            integer(c_long_long), value :: nanos
        end subroutine pb_sleep_

        function arc_str_new_(ptr, len) result(out) bind(C, name = "arc_str_new")
            use iso_c_binding, only: c_ptr, c_long_long
            import :: ArcFFIStrRaw
            type(ArcFFIStrRaw) :: out
            type(c_ptr), value :: ptr
            integer(c_long_long), value :: len
        end function arc_str_new_

        function arc_str_clone_(arc_str) result(out) bind(C, name = "arc_str_clone")
            use iso_c_binding, only: c_ptr, c_long_long
            import :: ArcFFIStrRaw
            type(ArcFFIStrRaw), value :: arc_str
            type(ArcFFIStrRaw) :: out
        end function arc_str_clone_

        subroutine arc_str_drop_(arc_str) bind(C, name = "arc_str_drop")
            use iso_c_binding, only: c_ptr, c_long_long
            import :: ArcFFIStrRaw
            type(ArcFFIStrRaw), intent(inout) :: arc_str
        end subroutine arc_str_drop_

        function pb_new_() result(pb) bind(C, name = "pb_new")
            import :: PrettyBenchRaw
            type(PrettyBenchRaw) :: pb
        end function pb_new_

        subroutine pb_new_group_individual_(pb, name) bind(C, name = "pb_new_group_individual")
            use iso_c_binding, only: c_long_long
            import :: PrettyBenchRaw, FFIStrRaw
            type(PrettyBenchRaw), value :: pb
            type(FFIStrRaw), value :: name
        end subroutine pb_new_group_individual_
        
        subroutine pb_new_group_bucketed_(pb, name, bucket_width_nanos) bind(C, name = "pb_new_group_bucketed")
            use iso_c_binding, only: c_long_long
            import :: PrettyBenchRaw, FFIStrRaw
            type(PrettyBenchRaw), value :: pb
            type(FFIStrRaw), value :: name
            integer(c_long_long), value :: bucket_width_nanos
        end subroutine pb_new_group_bucketed_

        function pb_start_bench_(pb, name) result(id) bind(C, name = "pb_start_bench")
            use iso_c_binding, only: c_long_long
            import :: PrettyBenchRaw, FFIStrRaw
            type(PrettyBenchRaw), value :: pb
            type(FFIStrRaw), value :: name
            integer(c_long_long) :: id
        end function pb_start_bench_

        subroutine pb_end_bench_(pb, name, id) bind(C, name = "pb_end_bench")
            use iso_c_binding, only: c_long_long
            import :: PrettyBenchRaw, FFIStrRaw
            type(PrettyBenchRaw), value :: pb
            type(FFIStrRaw), value :: name
            integer(c_long_long), value :: id
        end subroutine pb_end_bench_

        subroutine pb_serialise_to_file_(pb, dest) bind(C, name = "pb_serialise_to_file")
            use iso_c_binding, only: c_long_long
            import :: PrettyBenchRaw, FFIStrRaw
            type(PrettyBenchRaw), value :: pb
            type(FFIStrRaw), value :: dest
        end subroutine pb_serialise_to_file_

        subroutine pb_serialise_append_to_file_(pb, dest) bind(C, name = "pb_serialise_append_to_file")
            use iso_c_binding, only: c_long_long
            import :: PrettyBenchRaw, FFIStrRaw
            type(PrettyBenchRaw), value :: pb
            type(FFIStrRaw), value :: dest
        end subroutine pb_serialise_append_to_file_

        subroutine pb_print_histograms_(pb) bind(C, name = "pb_print_histograms")
            import :: PrettyBenchRaw, FFIStrRaw
            type(PrettyBenchRaw), value :: pb
        end subroutine pb_print_histograms_

        subroutine pb_import_from_file_(pb, src) bind(C, name = "pb_import_from_file")
            use iso_c_binding, only: c_long_long
            import :: PrettyBenchRaw, FFIStrRaw
            type(PrettyBenchRaw), value :: pb
            type(FFIStrRaw), value :: src
        end subroutine pb_import_from_file_

        subroutine pb_drop_(pb) bind(C, name = "pb_drop")
            import :: PrettyBenchRaw
            type(PrettyBenchRaw), value :: pb
        end subroutine pb_drop_

        function pb_clone_(pb) result(cloned_pb) bind(C, name = "pb_clone")
            import :: PrettyBenchRaw
            type(PrettyBenchRaw), value :: pb
            type(PrettyBenchRaw) :: cloned_pb
        end function 
    end interface
contains
    subroutine pb_sleep(nanos)
        use iso_fortran_env, only: int64
        integer(int64) :: nanos
        call pb_sleep_(nanos)
    end subroutine pb_sleep

    ! FFIStr conversions with ffi_str(...)
    function ffi_str_scalar(input_str) result(res)
        character(len=*), target, intent(in) :: input_str
        type(FFIStr) :: res

        res%inner%len = int(len(input_str), c_long_long)
        res%inner%ptr = c_loc(input_str)
    end function ffi_str_scalar

    function ffi_str_array(input_str) result(res)
        character, dimension(:), target, intent(in) :: input_str
        type(FFIStr) :: res

        res%inner%len = int(len(input_str), c_long_long)
        res%inner%ptr = c_loc(input_str)
    end function ffi_str_array

    function ffi_str_arc_str(input_str) result(res)
        type(ArcStr), intent(in) :: input_str
        type(FFIStr) :: res

        res%inner%ptr = input_str%inner%ptr
        res%inner%len = input_str%inner%len
    end function ffi_str_arc_str
    function ffi_str_ffi_str(input_str) result(res)
        type(FFIStr), intent(in) :: input_str
        type(FFIStr) :: res

        res = input_str
    end function ffi_str_ffi_str

    ! Procedure to create an f_str from 
    function ffi_str_as_f_str(input_str) result(res)
        class(FFIStr), intent(in) :: input_str
        character, dimension(:), pointer :: res
        integer :: n

        n = int(input_str%inner%len)
        call c_f_pointer(input_str%inner%ptr, res, [n])
    end function ffi_str_as_f_str

    ! ArcStr conversions with arc_str(...)
    function arc_str_ffi_str(input_str) result(res)
        type(FFiStr), intent(in) :: input_str
        type(ArcStr) :: res

        res%inner = arc_str_new_(input_str%inner%ptr, input_str%inner%len)
    end function arc_str_ffi_str

    function arc_str_scalar(input_str) result(res)
        character(len=*), target, intent(in) :: input_str
        type(ArcStr) :: res
        res = arc_str(ffi_str(input_str))
    end function arc_str_scalar

    function arc_str_array(input_str) result(res)
        character, dimension(:), target, intent(in) :: input_str
        type(ArcStr) :: res
        res = arc_str(ffi_str(input_str))
    end function arc_str_array

    function arc_str_arc_str(input_str) result(res)
        type(ArcStr), intent(in) :: input_str
        type(ArcStr) :: res
        res = input_str%clone()
    end function arc_str_arc_str

    function arc_str_raw_to_str(arc_str) result(res)
        type(ArcFFIStrRaw), value :: arc_str
        type(FFIStrRaw) :: res

        res%ptr = arc_str%ptr
        res%len = arc_str%len
    end function arc_str_raw_to_str

    function f_char_array_to_str_raw(f_char_array) result(str_raw)
        character(len=:), pointer, intent(in) :: f_char_array
        type(FFIStrRaw) :: str_raw

        str_raw%ptr = c_loc(f_char_array)
        str_raw%len = len(f_char_array)
    end function f_char_array_to_str_raw

    function arc_str_as_f_str(self) result(res)
        class(ArcStr), intent(in) :: self
        character, dimension(:), pointer :: res
        integer, dimension(1) :: shape

        shape(1) = int(self%inner%len)

        call c_f_pointer(self%inner%ptr, res, shape)
    end function arc_str_as_f_str

    function arc_str_clone(self) result(res)
        class(ArcStr), intent(in) :: self
        type(ArcStr) :: res

        res%inner = arc_str_clone_(self%inner)
    end function arc_str_clone
    
    subroutine arc_str_drop(self)
        class(ArcStr), intent(inout) :: self
        call arc_str_drop_(self%inner)
    end subroutine arc_str_drop


    function pb_new() result(res)
        type(PrettyBench) res
        res%inner = pb_new_()
    end function pb_new

    subroutine pb_serialise_to_file(self, dest)
        class(PrettyBench), intent(in) :: self
        type(FFIStr), value :: dest

        call pb_serialise_to_file_(self%inner, dest%inner)
    end subroutine pb_serialise_to_file

    subroutine pb_serialise_append_to_file(self, dest)
        class(PrettyBench), intent(in) :: self
        type(FFIStr), value :: dest

        call pb_serialise_append_to_file_(self%inner, dest%inner)
    end subroutine pb_serialise_append_to_file

    subroutine pb_print_histograms(self)
        class(PrettyBench), intent(in) :: self

        call pb_print_histograms_(self%inner)
    end subroutine pb_print_histograms

    subroutine pb_import_from_file(self, src)
        class(PrettyBench), intent(in) :: self
        type(FFIStr), value :: src

        call pb_import_from_file_(self%inner, src%inner)
    end subroutine pb_import_from_file


    subroutine pb_drop(self)
        class(PrettyBench), intent(in) :: self
        call pb_drop_(self%inner)
    end subroutine pb_drop

    function pb_clone(self) result(res)
        class(PrettyBench), intent(in) :: self
        type(PrettyBench) :: res

        res%inner = pb_clone_(self%inner)
    end function pb_clone

    subroutine pb_new_group(self, name, bucket_width_nanos)
        use iso_fortran_env, only: int64
        class(PrettyBench), intent(in) :: self
        type(ArcStr), intent(in) :: name
        integer(kind=int64), value, optional :: bucket_width_nanos

        if (present(bucket_width_nanos)) then
            call pb_new_group_bucketed_(self%inner, arc_str_raw_to_str(name%inner), int(bucket_width_nanos, c_long_long))
        else
            call pb_new_group_individual_(self%inner, arc_str_raw_to_str(name%inner))
        end if

        ! name%inner%ptr = c_null_ptr
        ! name%inner%len = 0

    end subroutine pb_new_group

    subroutine pb_end_bench(self, name, id)
        use iso_fortran_env, only: int64
        class(PrettyBench), intent(in) :: self
        type(FFIStr), value :: name
        integer(c_long_long) :: id
        
        call pb_end_bench_(self%inner, name%inner, id)
    end subroutine pb_end_bench

    function pb_start_bench(self, name) result(id)
        use iso_fortran_env, only: int64
        class(PrettyBench), intent(in) :: self
        type(FFIStr), value :: name
        integer(c_long_long) :: id
        
        id = pb_start_bench_(self%inner, name%inner)
    end function pb_start_bench

end module pretty_bench