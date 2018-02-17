program aqcomptran

  use globals
  use findstar
  use filters
  use align

  implicit none
  character(1024) :: fn, fntemp
  integer :: i
  real(sp), dimension(:,:), allocatable :: im, imref, imali, imsta
  type(starstruct), dimension(:), allocatable :: lst0, lst
  integer :: bsize, ndim, errno = 0
  logical :: anyf
  integer :: sz(2)
  real(dp) :: mx(2,3)

  iterate_arguments: do i = 1, command_argument_count()
    call get_command_argument(i, fn)

    write (0,'("<",A,">")') trim(fn)
    call ftdkopn(33, trim(fn), 0, bsize, errno)
    call ftrprt("stderr", errno)
    if (errno /= 0) error stop "problem opening FITS file"

    call ftgidm(33, ndim, errno)
    if (ndim /= 2) error stop "only monochrome images are supported for now"

    ! get image dimensions
    call ftgisz(33, 2, sz, errno)
    ! allocate memory and read the image
    allocate(im(sz(1), sz(2)))
    call ftgpve(33, 1, 1, product(sz), 0, im, anyf, errno)

    if (i == 1) then
      call findstar_local(im, lst0)
      imref = im
      imsta = im
    else
      call findstar_local(im, lst)
      call align_xyr(lst0, lst, mx)
      write (*,'("MATRIX = ",3F10.5)') mx(1,:), mx(2,:)

      if (.not. allocated(imali)) allocate(imali(sz(1), sz(2)))
      call improject(im, mx, imali)
      imsta = imsta + imali
      write(fntemp, '("diff.",I0,".fits")') i
      call fits_qsave(trim(fntemp), imali - imref)
    end if

    if (errno /= 0) error stop "problem reading image file"
    call ftclos(33, errno)
    if (allocated(im)) deallocate(im)

  end do iterate_arguments

  call fits_qsave("stack.fits", imsta)

  if (allocated(imref)) deallocate(imref)

contains

  subroutine findstar_local(im, list)
    real(sp), intent(in) :: im(:,:)
    type(starstruct), dimension(:), intent(inout), allocatable :: list
    type(starstruct) :: templist(2048)
    character(128) :: fn
    integer, save :: ifram = 1

    real :: krn(21,21), mean, thr
    real, allocatable :: im2(:,:)
    integer :: npix, nstars

    npix = size(im,1) * size(im,2)
    allocate(im2(size(im,1), size(im,2)))

    call wavelet_kernel(krn,2.0)
    call convol_dumb_trim(im,krn,im2)

    mean = sum(im2) / npix
    thr = sqrt(sum( (im2 - mean)**2 ) / npix) * 3 + mean
    where (im2 .lt. thr)  im2 = 0

    write (fn, '("stars.",I0,".fits")') ifram
    call fits_qsave(fn, im2)
    ifram = ifram + 1

    call aqfindstar(im2,templist,nstars)
    list = templist(1:nstars)

    deallocate(im2)
  end subroutine

  subroutine fits_qsave(fn, im)
    character(*) :: fn
    real, dimension(:,:) :: im
    integer :: errno

    errno = 0
    write (0,*) fn

    call ftdkinit(66, fn, 1, errno)
    if (errno /= 0) then
      call ftrprt("stderr", errno)
      error stop
    end if

    call ftphpr(66, .true., -32, 2, [size(im,1), size(im,2)], &
          & 0, 1, .true., errno)
    call ftppre(66, 1, 1, size(im,1) * size(im,2), im, errno)
    call ftclos(66, errno)
  end subroutine

end program
