module png

    !------------------------------------------------------------------------------------!

    use iso_c_binding
    use globals, only: fp

    !------------------------------------------------------------------------------------!

    implicit none
    private
    public write_png

    !------------------------------------------------------------------------------------!

    integer(c_int), parameter :: PNG_FREE_ALL = int(z'ffff', c_int)
    integer(c_int), parameter :: PNG_COLOR_MASK_PALETTE = 1, &
    &     PNG_COLOR_MASK_COLOR = 2, PNG_COLOR_MASK_ALPHA = 4
    integer(c_int), parameter :: PNG_COLOR_TYPE_GRAY = 0, &
    &     PNG_COLOR_TYPE_RGB = PNG_COLOR_MASK_COLOR
    integer(c_int), parameter :: PNG_COMPRESSION_TYPE_BASE = 0
    integer(c_int), parameter :: PNG_TEXT_COMPRESSION_NONE = -1
    integer(c_int), parameter :: PNG_INTERLACE_NONE = 0
    integer(c_int), parameter :: PNG_FILTER_TYPE_BASE = 0
    character(kind = c_char, len = *), parameter :: &
        PNG_LIBPNG_VER_STRING = "1.6.34" // char(0)

    !------------------------------------------------------------------------------------!

    interface
        subroutine fclose(h) bind(C)
            import c_int, c_ptr
            type(c_ptr), value :: h
        end subroutine
        function png_create_write_struct(ver, errptr, errfn, warnfn) result(png_ptr) bind(C)
            import c_char, c_ptr
            character(c_char), dimension(*) :: ver
            type(c_ptr), intent(in), value :: errptr, errfn, warnfn
            type(c_ptr) :: png_ptr
        end function
        function png_create_info_struct(png_ptr) result(info_ptr) bind(C)
            import c_ptr
            type(c_ptr), intent(in), value :: png_ptr
            type(c_ptr) :: info_ptr
        end function
        subroutine png_init_io(png_ptr, fptr) bind(C)
            import c_ptr
            type(c_ptr), intent(in), value :: png_ptr, fptr
        end subroutine
        subroutine png_set_ihdr(png_ptr, info_ptr, w, h, bits, color, interlace, &
            compress, filter) bind(C, name = 'png_set_IHDR')
            import c_ptr, c_int, c_int32_t
            type(c_ptr), intent(in), value :: png_ptr, info_ptr
            integer(c_int32_t), intent(in), value :: w, h, bits
            integer(c_int), intent(in), value :: color, interlace, compress, filter
        end subroutine
        subroutine png_write_info(png_ptr, info_ptr) bind(C)
            import c_ptr
            type(c_ptr), intent(in), value :: png_ptr, info_ptr
        end subroutine
        subroutine png_write_row(png_ptr, bytes) bind(C)
            import c_ptr
            type(c_ptr), intent(in), value :: png_ptr, bytes
        end subroutine
        subroutine png_write_end(png_ptr, info_ptr) bind(C)
            import c_ptr
            type(c_ptr), intent(in), value :: png_ptr, info_ptr
        end subroutine
        subroutine png_free_data(png_ptr, info_ptr, freeme, num) bind(C)
            import c_ptr, c_int, c_int32_t
            type(c_ptr), intent(in), value :: png_ptr, info_ptr
            integer(c_int32_t), intent(in), value :: freeme
            integer(c_int), intent(in), value :: num
        end subroutine
        subroutine png_destroy_write_struct(png_ptr, info_ptr) bind(C)
            import c_ptr
            type(c_ptr), intent(in), value :: png_ptr, info_ptr
        end subroutine
    end interface

contains

    !------------------------------------------------------------------------------------!

    subroutine fopen(fn, m, h)
        character(len = *), intent(in) :: fn, m
        type(c_ptr), intent(out) :: h
        interface
            type(c_ptr) function fopen_c(fn,m) bind(C, name = 'fopen')
                import c_char, c_ptr
                character(c_char), dimension(*) :: fn, m
            end function
        end interface
        h = fopen_c(trim(fn) // char(0), trim(m) // char(0))
    end subroutine

    !------------------------------------------------------------------------------------!

    subroutine write_png(fn, im, bits, errno)
        character(len = *), intent(in) :: fn
        real(fp), intent(in) :: im(:,:,:)
        integer, intent(in), optional :: bits
        integer, intent(inout), optional :: errno

        type(c_ptr) :: fptr, png_ptr, info_ptr
        integer(c_int8_t), allocatable, target :: row8(:)
        integer(c_int16_t), allocatable, target :: row16(:)
        integer :: i, j, nx, ny, nc, bits_
        integer(c_int) :: clrtype

        nx = size(im,2)
        ny = size(im,1)
        nc = size(im,3)

        bits_ = 8
        if (present(bits)) bits_ = bits
        if (bits_ /= 8 .and. bits_ /= 16) error stop "bits must be 8 or 16"

        select case (nc)
          case(1)
            clrtype = PNG_COLOR_TYPE_GRAY
          case(2)
            clrtype = ior(PNG_COLOR_TYPE_GRAY, PNG_COLOR_MASK_ALPHA)
          case(3)
            clrtype = PNG_COLOR_TYPE_RGB
          case(4)
            clrtype = ior(PNG_COLOR_TYPE_RGB, PNG_COLOR_MASK_ALPHA)
          case default
            error stop "size(im,3) must be: 1 (k), 2 (k+a) 3 (rgb) or 4 (rgb+a)"
        end select

        if (present(errno)) errno = 0

        try_write: block

            call fopen(fn, 'wb', fptr)
            if (.not.c_associated(fptr)) then
                if (.not. present(errno)) error stop
                errno = 1; exit try_write
            end if

            png_ptr = png_create_write_struct(PNG_LIBPNG_VER_STRING, &
                c_null_ptr, c_null_ptr, c_null_ptr)
            if (.not.c_associated(png_ptr)) then
                if (.not. present(errno)) error stop
                errno = 2; exit try_write
            end if

            info_ptr = png_create_info_struct(png_ptr)
            if (.not.c_associated(info_ptr)) then
                if (.not. present(errno)) error stop
                errno = 3; exit try_write
            end if

            call png_init_io(png_ptr, fptr);

            call png_set_ihdr(png_ptr, info_ptr, nx, ny, &
                bits_, clrtype, PNG_INTERLACE_NONE, &
                PNG_COMPRESSION_TYPE_BASE, PNG_FILTER_TYPE_BASE)

            call png_write_info(png_ptr, info_ptr)

            do j = 1, ny
                if (bits_ == 8) then
                    row8 = sc8(reshape(transpose(im(j,:,:)), [nx * nc]))
                    call png_write_row(png_ptr, c_loc(row8))
                else
                    row16 = sc16(reshape(transpose(im(j,:,:)), [nx * nc]))
                    call png_write_row(png_ptr, c_loc(row16))
                end if
            end do

            call png_write_end(png_ptr, c_null_ptr)
        end block try_write

        if (c_associated(fptr)) call fclose(fptr)
        if (c_associated(info_ptr)) call png_free_data(png_ptr, info_ptr, PNG_FREE_ALL, -1)
        if (c_associated(png_ptr)) call png_destroy_write_struct(png_ptr, c_null_ptr)

    end subroutine

    !------------------------------------------------------------------------------------!

    elemental function sc8(x) result(y)
        real(fp), intent(in) :: x
        integer(c_int8_t) :: y
        integer, parameter :: ymax = 2 * huge(y) + 1
        y = transfer(max(0, min(ymax, nint(x * ymax))), y)
    end function

    elemental function sc16(x) result(y)
        real(fp), intent(in) :: x
        integer(c_int16_t) :: y
        integer, parameter :: ymax = 2 * huge(y) + 1
        y = transfer(max(0, min(ymax, nint(x * ymax))), y)
    end function

    !------------------------------------------------------------------------------------!

end module
