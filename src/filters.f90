module filters

  use globals
  implicit none

contains

  !----------------------------------------------------------------------------!

  subroutine mexha(sg, k)
    real, intent(in) :: sg
    real, intent(out) :: k(:,:)
    integer :: i,j

    do concurrent (i = 1:size(k,1), j = 1:size(k,2))
      k(i,j) = mexha0(i - real(size(k,1) + 1) / 2,   &
                      j - real(size(k,2) + 1) / 2, sg)
    end do

  contains

    elemental function mexha0(x,y,sg) result(yf)
      real, intent(in) :: x,y,sg
      real :: yf,k
      real(dp), parameter :: pi = 4 * atan(1d0)
      k = (x**2 + y**2) / (2 * sg**2)
      yf =  (1 - k)  / (pi * sg**4 )  * exp( -k )
    end function
  end subroutine

  !----------------------------------------------------------------------------!
  ! quickselect algorithm
  ! translated from: Numerical recipes in C

  function quickselect(arr, k) result(median)
    real(dp), intent(inout) :: arr(:)
    real(dp) :: a, median
    integer, intent(in) :: k
    integer :: i,ir,j,l,mid

    l = 0
    ir = size(arr) - 1

    main_loop: do
      if (ir <= l+1) then

        if (ir == l+1 .and. arr(ir) < arr(l)) call swap(arr(l),arr(ir))
        median = arr(k)
        exit main_loop

      else

        mid = (l + ir) / 2
        call swap(arr(mid), arr(l+1))
        if (arr(l  ) > arr(ir )) call swap(arr(l  ), arr(ir ))
        if (arr(l+1) > arr(ir )) call swap(arr(l+1), arr(ir ))
        if (arr(l  ) > arr(l+1)) call swap(arr(l  ), arr(l+1))

        i = l + 1
        j = ir
        a = arr(l+1)

        inner: do
          do
            i = i + 1
            if (arr(i) >= a) exit
          end do
          do
            j = j - 1
            if (arr(j) <= a) exit
          end do
        	if (j < i) exit inner
        	call swap(arr(i), arr(j))
        end do inner

        arr(l+1) = arr(j)
        arr(j) = a

        if (j >= k) ir = j - 1
        if (j <= k) l = i
      end if
    end do main_loop

  contains

    elemental subroutine swap(a,b)
      real(dp), intent(inout) :: a, b
      real(dp) :: c
      c = a
      a = b
      b = c
    end subroutine
  end function

  !----------------------------------------------------------------------------!

  subroutine outliers(im, sigma, niter, msk)
    use ieee_arithmetic, only: ieee_is_normal

    real(sp), intent(in) :: im(:,:), sigma
    integer, intent(in) :: niter
    logical, intent(out) :: msk(:,:)
    integer :: i,nn
    real(dp) :: mean, stdev

    msk(:,:) = ieee_is_normal(im)

    do i = 1, niter
      nn    = count(msk)
      mean  = sum(im, msk) / nn
      stdev = sqrt(sum((im - mean)**2, msk) / nn)

      msk = msk &
          .and. (im >= mean - sigma * stdev) &
          .and. (im <= mean + sigma * stdev)

      if (cfg_verbose) write (0, '(I2, " / ", I2, 3X, I10, 2F10.1)') &
        i, niter, nn, mean, stdev
    end do

  end subroutine

  !----------------------------------------------------------------------------!

  subroutine convol_dumb(img,krn,imgout)
    real, intent(in) :: img(:,:), krn(:,:)
    real, intent(out) :: imgout(size(img,1),size(img,2))
    integer :: x,y,szx,szy, rx, ry, kx, ky
    integer ::  x1, y1, x2, y2
    integer ::  x1k, y1k, x2k, y2k
    szx = size(img,1)
    szy = size(img,2)
    kx = size(krn,1)
    ky = size(krn,2)
    if ( mod(kx,2) .eq. 0 .or. mod(ky,2) .eq. 0 ) then
      stop "Kernel must have uneven dimension"
    end if
    rx = floor(kx * 0.5)
    ry = floor(ky * 0.5)
    do y=1,szy
      do x=1,szx
        x1 = max( 1, x-rx )
        x1k = 1 + x1 - (x-rx)
        x2 = min( szx, x+rx )
        x2k = kx + x2 - (x+rx)
        y1 = max( 1, y-ry )
        y1k = 1 + y1 - (y-ry)
        y2 = min( szy, y+ry )
        y2k = ky + y2 - (y+ry)
        imgout(x,y) = sum( krn(x1k:x2k,y1k:y2k) * img(x1:x2,y1:y2) )
      end do
    end do

  end subroutine

  !----------------------------------------------------------------------------!

  subroutine convol_dumb_trim(img,krn,imgout)
    real, intent(in) :: img(:,:), krn(:,:)
    real, intent(out) :: imgout(size(img,1),size(img,2))
    integer :: x,y,szx,szy, rx, ry, kx, ky
    integer ::  x1, y1, x2, y2
    integer ::  x1k, y1k, x2k, y2k
    szx = size(img,1)
    szy = size(img,2)
    kx = size(krn,1)
    ky = size(krn,2)
    if ( mod(kx,2) .eq. 0 .or. mod(ky,2) .eq. 0 ) then
      stop "Kernel must have uneven dimension"
    end if
    imgout = 0
    rx = floor(kx * 0.5)
    ry = floor(ky * 0.5)
    do y=1+ry,szy-ry
      do x=1+rx,szx-rx
        imgout(x,y) = sum( krn * img((x-rx):(x+rx),(y-ry):(y+ry)) )
      end do
    end do

  end subroutine

end module
