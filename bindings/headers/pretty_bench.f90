module pretty_bench
    use iso_fortran_env, only: int32, int64, real64
    use iso_c_binding, only: c_ptr, c_int32_t, c_int64_t, c_loc, c_f_pointer, c_null_ptr
    implicit none
    private
    public :: pb_sleep, pb_new, t_pretty_bench, arc_str, t_arc_str, ffi_str, t_duration, duration_nanos, duration_micros, duration_millis, operator (+), operator (-)

    type, bind(C) :: t_duration_raw
        integer(c_int32_t) :: nanos
        integer(c_int64_t) :: secs
    end type t_duration_raw

    type, bind(C) :: t_ffi_str_raw
        type(c_ptr) :: ptr = c_null_ptr
        integer(c_int64_t) :: len = 0
    end type t_ffi_str_raw

    type, bind(C) :: t_arc_str_raw
        type(c_ptr) :: ptr = c_null_ptr
        integer(c_int64_t) :: len = 0
    end type t_arc_str_raw

    type t_duration
        type(t_duration_raw) :: inner
    contains
        procedure :: as_secs => duration_as_secs, &
                     as_secs_real64 => duration_as_secs_real64, &
                     as_nanos => duration_as_nanos
    end type t_duration

    type t_ffi_str
        type(t_ffi_str_raw) :: inner
    contains
        procedure :: as_f_str => ffi_str_as_f_str
    end type t_ffi_str

    type t_arc_str
        type(t_arc_str_raw) :: inner
    contains
        procedure :: clone => arc_str_clone, &
                     drop => arc_str_drop, &
                     as_f_str => arc_str_as_f_str
    end type t_arc_str

    type, bind(C) :: t_pretty_bench_raw
        type(c_ptr) :: inner
    end type t_pretty_bench_raw
    
    type t_pretty_bench
        type(t_pretty_bench_raw) :: inner
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
    end type t_pretty_bench

    interface operator(+)
        procedure :: add_durations
    end interface

    interface operator(-)
        procedure :: sub_durations
    end interface

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
        subroutine pb_sleep_(dur) bind(C, name = "pb_sleep")
            import :: t_duration_raw
            type(t_duration_raw), value :: dur
        end subroutine pb_sleep_

        function arc_str_new_(ptr, len) result(out) bind(C, name = "arc_str_new")
            use iso_c_binding, only: c_ptr, c_int64_t
            import :: t_arc_str_raw
            type(t_arc_str_raw) :: out
            type(c_ptr), value :: ptr
            integer(c_int64_t), value :: len
        end function arc_str_new_

        function arc_str_clone_(arc_str) result(out) bind(C, name = "arc_str_clone")
            import :: t_arc_str_raw
            type(t_arc_str_raw), value :: arc_str
            type(t_arc_str_raw) :: out
        end function arc_str_clone_

        subroutine arc_str_drop_(arc_str) bind(C, name = "arc_str_drop")
            import :: t_arc_str_raw
            type(t_arc_str_raw), intent(inout) :: arc_str
        end subroutine arc_str_drop_

        function pb_new_() result(pb) bind(C, name = "pb_new")
            import :: t_pretty_bench_raw
            type(t_pretty_bench_raw) :: pb
        end function pb_new_

        subroutine pb_new_group_individual_(pb, name) bind(C, name = "pb_new_group_individual")
            import :: t_pretty_bench_raw, t_ffi_str_raw
            type(t_pretty_bench_raw), value :: pb
            type(t_ffi_str_raw), value :: name
        end subroutine pb_new_group_individual_
        
        subroutine pb_new_group_bucketed_(pb, name, bucket_width) bind(C, name = "pb_new_group_bucketed")
            import :: t_pretty_bench_raw, t_ffi_str_raw, t_duration_raw
            type(t_pretty_bench_raw), value :: pb
            type(t_ffi_str_raw), value :: name
            type(t_duration_raw), value :: bucket_width
        end subroutine pb_new_group_bucketed_

        function pb_start_bench_(pb) result(start) bind(C, name = "pb_start_bench")
            import :: t_pretty_bench_raw, t_ffi_str_raw, t_duration_raw
            type(t_pretty_bench_raw), value :: pb
            type(t_duration_raw) :: start
        end function pb_start_bench_

        subroutine pb_end_bench_(pb, name, start) bind(C, name = "pb_end_bench")
            import :: t_pretty_bench_raw, t_ffi_str_raw, t_duration_raw
            type(t_pretty_bench_raw), value :: pb
            type(t_ffi_str_raw), value :: name
            type(t_duration_raw), value :: start
        end subroutine pb_end_bench_

        subroutine pb_serialise_to_file_(pb, dest) bind(C, name = "pb_serialise_to_file")
            import :: t_pretty_bench_raw, t_ffi_str_raw
            type(t_pretty_bench_raw), value :: pb
            type(t_ffi_str_raw), value :: dest
        end subroutine pb_serialise_to_file_

        subroutine pb_serialise_append_to_file_(pb, dest) bind(C, name = "pb_serialise_append_to_file")
            import :: t_pretty_bench_raw, t_ffi_str_raw
            type(t_pretty_bench_raw), value :: pb
            type(t_ffi_str_raw), value :: dest
        end subroutine pb_serialise_append_to_file_

        subroutine pb_print_histograms_(pb) bind(C, name = "pb_print_histograms")
            import :: t_pretty_bench_raw, t_ffi_str_raw
            type(t_pretty_bench_raw), value :: pb
        end subroutine pb_print_histograms_

        subroutine pb_import_from_file_(pb, src) bind(C, name = "pb_import_from_file")
            import :: t_pretty_bench_raw, t_ffi_str_raw
            type(t_pretty_bench_raw), value :: pb
            type(t_ffi_str_raw), value :: src
        end subroutine pb_import_from_file_

        subroutine pb_drop_(pb) bind(C, name = "pb_drop")
            import :: t_pretty_bench_raw
            type(t_pretty_bench_raw), intent(inout) :: pb
        end subroutine pb_drop_

        function pb_clone_(pb) result(cloned_pb) bind(C, name = "pb_clone")
            import :: t_pretty_bench_raw
            type(t_pretty_bench_raw), value :: pb
            type(t_pretty_bench_raw) :: cloned_pb
        end function 
    end interface
contains
    type(t_duration) function add_durations(left, right) result(res)
        type(t_duration), intent(in) :: left, right

        integer(kind=int64) :: temp
        temp = int(left%inner%nanos, int64) + int(right%inner%nanos, int64)
        res%inner%nanos = int(mod(temp, 1000000000_int64), int32)
        res%inner%secs = temp / 1000000000_int64 + left%inner%secs + right%inner%secs
    end function add_durations

    type(t_duration) function sub_durations(left, right) result(res)
        type(t_duration), intent(in) :: left, right

        integer(kind=int64) :: temp
        temp = int(left%inner%nanos, int64) - int(right%inner%nanos, int64)
        res%inner%nanos = int(mod(temp, 1000000000_int64), int32)
        res%inner%secs = temp / 1000000000_int64 + left%inner%secs - right%inner%secs
    end function sub_durations

    type(t_duration) function duration_nanos(nanos) result(res)
        integer(kind=int64), intent(in) :: nanos

        res%inner%secs = nanos / 1000000000_int64
        res%inner%nanos = int(mod(nanos, 1000000000_int64), int32)
    end function duration_nanos

    type(t_duration) function duration_micros(micros) result(res)
        integer(kind=int64), intent(in) :: micros

        res = duration_nanos(micros * 1000_int64)
    end function duration_micros

    type(t_duration) function duration_millis(millis) result(res)
        integer(kind=int64), intent(in) :: millis

        res = duration_nanos(millis * 1000000_int64)
    end function duration_millis

    integer(int64) function duration_as_secs(self) result(res)
        class(t_duration), intent(in) :: self
        res = self%inner%secs
    end function duration_as_secs

    real(real64) function duration_as_secs_real64(self) result(res)
        class(t_duration), intent(in) :: self
        res = real(self%inner%secs, real64) + real(self%inner%nanos, real64) *1e-9
    end function duration_as_secs_real64

    integer(int64) function duration_as_nanos(self) result(res)
        class(t_duration), intent(in) :: self
        res = self%inner%secs * 1000000000_int64 + int(self%inner%nanos, int64)
    end function duration_as_nanos

    subroutine pb_sleep(dur)
        type(t_duration), intent(in) :: dur
        call pb_sleep_(dur%inner)
    end subroutine pb_sleep

    ! t_ffi_str conversions with ffi_str(...)
    type(t_ffi_str) function ffi_str_scalar(input_str) result(res)
        character(len=*), target, intent(in) :: input_str

        res%inner%len = int(len(input_str), c_int64_t)
        res%inner%ptr = c_loc(input_str)
    end function ffi_str_scalar

    type(t_ffi_str) function ffi_str_array(input_str) result(res)
        character, dimension(:), target, intent(in) :: input_str

        res%inner%len = int(len(input_str), c_int64_t)
        res%inner%ptr = c_loc(input_str)
    end function ffi_str_array

    type(t_ffi_str) function ffi_str_arc_str(input_str) result(res)
        type(t_arc_str), intent(in) :: input_str

        res%inner%ptr = input_str%inner%ptr
        res%inner%len = input_str%inner%len
    end function ffi_str_arc_str
    type(t_ffi_str) function ffi_str_ffi_str(input_str) result(res)
        type(t_ffi_str), intent(in) :: input_str

        res = input_str
    end function ffi_str_ffi_str

    ! Procedure to create an f_str from 
    function ffi_str_as_f_str(input_str) result(res)
        class(t_ffi_str), intent(in) :: input_str
        character, dimension(:), pointer :: res
        integer :: n

        n = int(input_str%inner%len)
        call c_f_pointer(input_str%inner%ptr, res, [n])
    end function ffi_str_as_f_str

    ! t_arc_str conversions with arc_str(...)
    type(t_arc_str) function arc_str_ffi_str(input_str) result(res)
        type(t_ffi_str), intent(in) :: input_str

        res%inner = arc_str_new_(input_str%inner%ptr, input_str%inner%len)
    end function arc_str_ffi_str

    type(t_arc_str) function arc_str_scalar(input_str) result(res)
        character(len=*), target, intent(in) :: input_str
        res = arc_str(ffi_str(input_str))
    end function arc_str_scalar

    type(t_arc_str) function arc_str_array(input_str) result(res)
        character, dimension(:), target, intent(in) :: input_str
        res = arc_str(ffi_str(input_str))
    end function arc_str_array

    type(t_arc_str) function arc_str_arc_str(input_str) result(res)
        type(t_arc_str), intent(in) :: input_str
        res = input_str%clone()
    end function arc_str_arc_str

    type(t_ffi_str_raw) function arc_str_raw_to_str(arc_str) result(res)
        type(t_arc_str_raw), value :: arc_str

        res%ptr = arc_str%ptr
        res%len = arc_str%len
    end function arc_str_raw_to_str

    type(t_ffi_str_raw) function f_char_array_to_str_raw(f_char_array) result(str_raw)
        character(len=:), pointer, intent(in) :: f_char_array

        str_raw%ptr = c_loc(f_char_array)
        str_raw%len = len(f_char_array)
    end function f_char_array_to_str_raw

    function arc_str_as_f_str(self) result(res)
        class(t_arc_str), intent(in) :: self
        character, dimension(:), pointer :: res
        integer, dimension(1) :: shape

        shape(1) = int(self%inner%len)

        call c_f_pointer(self%inner%ptr, res, shape)
    end function arc_str_as_f_str

    type(t_arc_str) function arc_str_clone(self) result(res)
        class(t_arc_str), intent(in) :: self

        res%inner = arc_str_clone_(self%inner)
    end function arc_str_clone
    
    subroutine arc_str_drop(self)
        class(t_arc_str), intent(inout) :: self
        call arc_str_drop_(self%inner)
    end subroutine arc_str_drop


    type(t_pretty_bench) function pb_new() result(res)
        res%inner = pb_new_()
    end function pb_new

    subroutine pb_serialise_to_file(self, dest)
        class(t_pretty_bench), intent(in) :: self
        type(t_ffi_str), value :: dest

        call pb_serialise_to_file_(self%inner, dest%inner)
    end subroutine pb_serialise_to_file

    subroutine pb_serialise_append_to_file(self, dest)
        class(t_pretty_bench), intent(in) :: self
        type(t_ffi_str), value :: dest

        call pb_serialise_append_to_file_(self%inner, dest%inner)
    end subroutine pb_serialise_append_to_file

    subroutine pb_print_histograms(self)
        class(t_pretty_bench), intent(in) :: self

        call pb_print_histograms_(self%inner)
    end subroutine pb_print_histograms

    subroutine pb_import_from_file(self, src)
        class(t_pretty_bench), intent(in) :: self
        type(t_ffi_str), value :: src

        call pb_import_from_file_(self%inner, src%inner)
    end subroutine pb_import_from_file


    subroutine pb_drop(self)
        class(t_pretty_bench), intent(inout) :: self
        call pb_drop_(self%inner)
    end subroutine pb_drop

    type(t_pretty_bench) function pb_clone(self) result(res)
        class(t_pretty_bench), intent(in) :: self

        res%inner = pb_clone_(self%inner)
    end function pb_clone

    subroutine pb_new_group(self, name, bucket_width)
        class(t_pretty_bench), intent(in) :: self
        type(t_arc_str), intent(in) :: name
        type(t_duration), value, optional :: bucket_width

        if (present(bucket_width)) then
            call pb_new_group_bucketed_(self%inner, arc_str_raw_to_str(name%inner), bucket_width%inner)
        else
            call pb_new_group_individual_(self%inner, arc_str_raw_to_str(name%inner))
        end if

        ! name%inner%ptr = c_null_ptr
        ! name%inner%len = 0

    end subroutine pb_new_group

    subroutine pb_end_bench(self, name, start)
        class(t_pretty_bench), intent(in) :: self
        type(t_ffi_str), value :: name
        type(t_duration) :: start
        
        call pb_end_bench_(self%inner, name%inner, start%inner)
    end subroutine pb_end_bench

    type(t_duration) function pb_start_bench(self) result(start)
        class(t_pretty_bench), intent(in) :: self
        
        start%inner = pb_start_bench_(self%inner)
    end function pb_start_bench

end module pretty_bench