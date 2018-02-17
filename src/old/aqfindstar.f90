program program_aqfindstar

    use findstar
    use filters
    use iso_fortran_env

    implicit none

    real(real32), allocatable :: img(:,:), img2(:,:), imdata(:,:,:)
    type(starstruct) :: list(2**16)
    integer :: i

    integer :: status, bsize, sz(3), ndim, nstars
    logical :: anyf
    ! real(real32), allocatable :: img(:,:)
    character(len=50) :: errstr, comment
    character(len=1200) :: fn
    character(len=1200) :: buf
    real :: krn(25,25), thr, mean

    call get_command_argument(1, fn)
    if (fn == '') stop "usage: aqfindstar <fn>"

    status = 0
    bsize = 1
    call ftopen(33,trim(fn),0,bsize,status)

    if ( status .ne. 0 ) then
        call ftgerr(status,errstr)
        write (0,*) 'error: ', trim(errstr)
        stop -1
    end if

    call ftgidm(33,ndim,status)
    if ( ndim .eq. 2 ) then
        write(0,*) 'Got monochrome image'
        call ftgisz(33,2,sz(1:2),status)
        write(0,'(I20,A,I0)') sz(1), ' x ', sz(2)
        sz(3) = 1
    else if ( ndim .eq. 3 ) then
        write(0,*) 'Got color image'
        call ftgisz(33,3,sz,status)
        write(0,'(I20,A,I0,A,I0)') sz(1), ' x ', sz(2), ' x ', sz(3)
    else
        stop "Unknown image dimension"
    end if

    ! allocate a generic 3D array
    allocate( imdata(sz(1),sz(2),sz(3)) )
    ! read the imdata from file
    call ftgpve(33,1,1,product(sz),0,imdata,anyf,status)
    call ftclos(33,status)

    allocate( img(sz(1),sz(2)) )
    allocate( img2(sz(1),sz(2)) )
    call wavelet_kernel(krn,3.0)
    img(:,:) = imdata(:,:,1)
    call convol_dumb_trim(img,krn,img2 )
    mean = sum(img2) / product(sz)
    thr = sqrt(sum( (img2-mean)**2 ) / product(sz)) + mean
    ! thr = maxval(img2) * 0.025
    where (img2 .lt. thr)  img2 = 0
    call aqfindstar(img2,list,nstars)
    deallocate(img2)

    write (6,'(2I10)') sz(1:2)
    write (6,'(I10)') nstars

    do i = 1,size(list)
        if ( list(i) % v .lt. 0 ) exit
        write (6,'(3F15.2)') list(i) % v, list(i) % x, list(i) % y
    end do
    deallocate(img)

contains

    subroutine aqfindstar_wrapper(img)
        real(real32), intent(in) :: img(:,:)
        integer, parameter :: krnsz = 17
        real(real32) :: img2(size(img,1),size(img,2)), krn(krnsz,krnsz)
    end subroutine

end program
