program test_legacy_align

  use globals
  use source_m, only: source_t
  use legacy_align

  implicit none
  character(1024) :: fn, fntemp
  integer :: i
  real(r32_k), allocatable :: im(:,:), imali(:,:,:)
  real(r32_k), allocatable :: imsta(:,:)
  type(source_t), dimension(:), allocatable :: lst0, lst
  integer :: bsize, ndim, errno = 0
  logical :: anyf
  integer :: sz(2)
  real(r32_k) :: mx(2,3)

  if (command_argument_count() == 0) then
    print *, 'usage: test_legacy_align <file1.fits> <file2.fits> ...'
    stop
  end if

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
    if (errno /= 0) error stop
    ! close
    call ftclos(33, errno)

    if (i == 1) then
      if (.not. allocated(imsta)) allocate(imsta(sz(1), sz(2)))
      if (.not. allocated(imali)) &
        allocate(imali(sz(1), sz(2), command_argument_count()))
      call findstar_local(im, lst0)
      imsta(:,:) = im(:,:)
      imali(:,:,1) = im(:,:)
    else
      call findstar_local(im, lst)
      call align_xyr(lst0, lst, mx)
      write (*,'("MATRIX = ",3F10.5)') mx(1,:), mx(2,:)

      call improject(im, mx, imali(:,:,i))
      imsta(:,:) = imsta(:,:) + imali(:,:,i)
    end if

    if (allocated(im)) deallocate(im)

  end do iterate_arguments

  open(12, file = 'regions.txt', action = 'write')
  do i = 1, size(lst0)
    write (12, *) 'circle', lst0(i) % x, lst0(i) % y, lst0(i) % rms * 3.75
  end do
  close(12)

  write_stack: block
    call ftdkinit(66, "stack.fits", 1, errno)
    if (errno /= 0) error stop

    call ftphps(66, -storage_size(real(1, r32_k)), 2, shape(imsta), errno)
    call ftppre(66, 1, 1, size(imsta), real(imsta / size(imali,3), r32_k), errno)
    call ftclos(66, errno)
  end block write_stack

  write_aligned: block
    call ftdkinit(67, "aligned.fits", 1, errno)
    if (errno /= 0) error stop

    call ftphps(67, -storage_size(real(1, r32_k)), 3, shape(imali), errno)
    call ftppre(67, 1, 1, size(imali), imali, errno)
    call ftclos(67, errno)
  end block write_aligned


contains

  subroutine findstar_local(im, lst)

    use convolutions, only: convol_fix
    use kernels, only: mexhakrn_alloc
    use findstar, only: aqfindstar

    real(r32_k), intent(in) :: im(:,:)
    type(source_t), intent(out), allocatable :: lst(:)
    real(r32_k), allocatable :: im2(:,:), krn(:,:)
    integer :: nstars

    krn = mexhakrn_alloc(2.3_r32_k)

    allocate(im2(size(im,1), size(im,2)))
    call convol_fix(im, krn, im2, 'r')
    call aqfindstar(im2, lst, limit = 256)

  end subroutine

end program
