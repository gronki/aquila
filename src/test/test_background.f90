program background

  use globals
  implicit none

  real(fp), dimension(:,:), allocatable :: im
  real(fp), dimension(:,:,:), allocatable :: imout
  integer :: bsize, ndim, errno = 0
  logical :: anyf
  integer :: sz(2)

  call ftdkopn(33, "in.fits", 0, bsize, errno)
  call ftrprt("stderr", errno)
  if (errno /= 0) error stop "problem opening FITS file"

  call ftgidm(33, ndim, errno)
  if (ndim /= 2) error stop "only monochrome images are supported for now"

  ! get image dimensions
  call ftgisz(33, 2, sz, errno)
  ! allocate memory and read the image
  allocate(im(sz(1), sz(2)))
  call ftgpve(33, 1, 1, product(sz), 0, im, anyf, errno)

  !----------------------------------------------------------------------------!

  main: block

    use statistics, only: outliers, quickselect, sigstd

    integer, parameter :: mrad = 15
    real(fp), dimension(4 * mrad**2 + 4 * mrad + 1) :: k
    logical :: mskt(2 * mrad + 1, 2 * mrad + 1)
    real(fp) :: blur(2 * mrad + 1, 2 * mrad + 1)
    integer i, j, ni, nj
    logical, allocatable :: msk(:,:)

    ni = size(im,1)
    nj = size(im,2)

    allocate(imout(ni, nj, 7))
    allocate(msk(ni, nj))
    imout(:,:,1) = im(:,:)

    prepare: block
      integer, parameter :: nb = 96
      real(fp), dimension(nb**2) :: k
      real(fp) :: mean, median, stdev
      integer :: ioff, joff

      ioff = mod(ni,nb) / 2
      joff = mod(nj,nb) / 2

      !$omp parallel do private(i,j,k,mean,median,stdev)
      do j = joff + 1, nj - nb + 1, nb
        do i = ioff + 1, ni - nb + 1, nb
          associate(imc =>  im(max(i, 1) : min(i + nb - 1, ni),     &
                               max(j, 1) : min(j + nb - 1, nj)),    &
                   mskc => msk(max(i, 1) : min(i + nb - 1, ni),     &
                               max(j, 1) : min(j + nb - 1, nj)),    &
                  imo => imout(max(i, 1) : min(i + nb - 1, ni),     &
                               max(j, 1) : min(j + nb - 1, nj), :))
            associate (n => size(imc))
              call outliers(imc, 5.0_fp, 16, mskc)
              call sigstd(imc, mean, stdev)
              imo(:,:,2) = imc(:,:) - mean
              ! imo(:,:,3) = imc(:,:) - (mean + stdev)
              k(1:n) = reshape(imc, [n])
              median = quickselect(k(1:n), (n + 1) / 2)
              imo(:,:,3) = imc(:,:) - median
              imo(:,:,4) = imc(:,:) - (3 * mean - 2 * median)
              imo(:,:,5) = merge(imc(:,:), 0.0_fp, imc >= mean + 5 * stdev)
            end associate
          end associate
        end do
      end do
      !$omp end parallel do
    end block prepare

    wave: block
      use kernels, only: gausskrn, mexhakrn
      use convolutions, only: convol, convol_fix
      ! fwhm = 2.355 * sigma
      ! kernel size for gaussian: 2.5 * fwhm
      real(fp) :: krn(7,7), krn2(3,3), krn3(15,15), stdev, mean
      real(fp), allocatable :: tmp(:,:)
      real(fp) :: fwhm = 3.0
      krn2 = reshape([0,-1,0,-1,4,-1,0,-1,0], shape(krn2))
      call gausskrn(fwhm / 2.355, krn)
      allocate(tmp, source = im)
      call convol_fix(im, krn2, tmp, 'R')
      call convol_fix(tmp, krn, imout(:,:,6), 'R')
      print '(7F10.5)', krn
      ! call outliers(im, 3.0, 16, msk, mean, stdev)
      ! where(msk .or. imout(:,:,6) < 0) imout(:,:,6) = 0
      call mexhakrn(fwhm / 2.355, krn3)
      call convol_fix(im, krn3, imout(:,:,7), 'R')

    end block wave
    !
    ! do concurrent (i = 2:ni-1, j = 2:nj-1)
    !   imout(i,j,6) = 0.5 *im(i,j) &
    !   + 0.125 * (im(i,j+1) + im(i,j-1) + im(i+1,j) + im(i-1,j)) ! - (im(i+1,j+1) + im(i-1,j+1) + im(i-1,j-1) + im(i+1,j-1))
    ! end do
    !
    ! do concurrent (i = 2:ni-1, j = 2:nj-1)
    !   imout(i,j,6) = 4 * im(i,j) &
    !   - (im(i,j+1) + im(i,j-1) + im(i+1,j) + im(i-1,j)) ! - (im(i+1,j+1) + im(i-1,j+1) + im(i-1,j-1) + im(i+1,j-1))
    ! end do


    ! ! call shrink_mask(msk,1)
    !
    ! !$omp parallel do private(i,j,k)
    ! do j = mrad + 1, size(im,2) - mrad
    !   do  i = mrad + 1, size(im,1) - mrad
    !     associate (imc =>  im(i - mrad : i + mrad, j - mrad : j + mrad), &
    !               mskc => msk(i - mrad : i + mrad, j - mrad : j + mrad))
    !       ! call outliers(imc, 3.0, 16, mskt)
    !       k(:) = reshape(imc, [(2 * mrad + 1)**2])
    !       imout(i,j,1) = merge(imout(i,j,1), 0.0_sp, msk(i,j))
    !       imout(i,j,2) = sum(imc * blur, mskc) / sum(blur, mskc)
    !       ! imout(i,j,2) = quickselect(k, 2 * mrad**2 + 2 * mrad + 1)
    !       imout(i,j,3) = sum(imc, mskc) / count(mskc)
    !
    !     end associate
    !   end do
    ! end do
    ! !$omp end parallel do


  end block main

  !----------------------------------------------------------------------------!

  call ftdkinit(66, "out.fits", 1, errno)
  if (errno /= 0) then
    call ftrprt("stderr", errno)
    error stop
  end if

  call ftphpr(66, .true., -32, 3, shape(imout), 0, 1, .true., errno)
  call ftppre(66, 1, 1, size(imout), imout, errno)
  call ftclos(66, errno)

contains

  subroutine shrink_mask(mask,r)

    logical, intent(inout) :: mask(:,:)
    logical, dimension(:,:), allocatable :: mask0
    integer, intent(in) :: r
    integer :: x, y, x0, x1, y0, y1, nx, ny

    nx = size(mask,1)
    ny = size(mask,2)

    mask0 = mask

    do y = 1,ny
      do x = 1,nx
        if ( .not. mask0(x,y) ) then
          x0 = max(x - r, 1)
          x1 = min(x + r, nx)
          y0 = max(y - r, 1)
          y1 = min(y + r, ny)
          mask(x0:x1,y0:y1) = .false.
        end if
      end do
    end do

    deallocate(mask0)

  end subroutine

end program
