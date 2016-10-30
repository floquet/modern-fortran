! https://stackoverflow.com/questions/19726193/linking-lapack-in-fortran-on-mac-os-x

program eigtest
    complex(kind=8),allocatable :: A(:,:), eigs(:), vecs(:,:)
    integer                     :: ierr

    allocate(A(3,3),stat=ierr)
    if (ierr /= 0) STOP "*** not enough memory ***"

    A(1,1) = cmplx(1,0)
    A(1,2) = cmplx(0,2)
    A(1,3) = cmplx(3,0)
    A(2,1) = cmplx(0,-2)
    A(2,2) = cmplx(5,0)
    A(2,3) = cmplx(1,-1)
    A(3,1) = cmplx(3,0)
    A(3,2) = cmplx(1,1)
    A(3,3) = cmplx(7,0)
    !call heevd(A, eigs)
    call wrapped_zheevd(A, eigs,vecs)
    write(*,*) eigs

    contains

subroutine wrapped_zheevd (matin, &
                         zvals,zvecs )
    integer                                  :: ndim
    complex(kind=8),intent(in),  allocatable :: matin(:,:)
    complex(kind=8),intent(out), allocatable :: zvals(:),zvecs(:,:)
    character*1                              :: jobz='V',uplo='U'
    integer                                  :: info,lda,liwork,lrwork,lwork,n
    integer,                     allocatable :: iwork(:)
    real(kind=8),                allocatable :: rwork(:), w(:)
    complex(kind=8),             allocatable :: A(:,:),   work(:)
    integer                                  :: ierr

    ndim=size(matin(1,:))

    if (allocated(zvecs)) deallocate(zvecs)
    if (allocated(zvals)) deallocate(zvals)
    allocate(zvecs(ndim,ndim),zvals(ndim),stat=ierr)
    if (ierr /= 0) STOP "*** not enough memory ***"

    n=ndim
    lda=n

    lwork  = 2*n+n**2
    lrwork = 1+5*n+2*n**2
    liwork = 3+5*n

    allocate(a(ndim,ndim),w(ndim),work(lwork),&
             rwork(lrwork),iwork(liwork),stat=ierr)
    if (ierr /= 0) STOP "*** not enough memory ***"

    a=matin

    call zheevd(jobz,uplo,n,a,lda,w,work,lwork,rwork,lrwork,iwork,liwork,info)

    zvals=w
    zvecs=a

    deallocate(a,w,rwork,iwork,work)

end subroutine

end

! Mathematica results
! {9.18348, 4.49485, -0.678322}

! dan-topas-pro-2:nla rditldmt$ date
! Fri Jan 15 14:34:18 CST 2016
! dan-topas-pro-2:nla rditldmt$ pwd
! /Users/rditldmt/Box Sync/fortran/demos/nla
! dan-topas-pro-2:nla rditldmt$ gfortran  -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -framework Accelerate -o eigtest eigtest.f08
! eigtest.f08:28:15:
!
!      character*1                              :: jobz='V',uplo='U'
!                1
! Warning: Obsolescent feature: Old-style character length at (1)
! dan-topas-pro-2:nla rditldmt$ ./eigtest
!  (-0.67832232182451535     ,  0.0000000000000000     ) (  4.4948459029047951     ,  0.0000000000000000     ) (  9.1834764189197191     ,  0.0000000000000000     )
! dan-topas-pro-2:nla rditldmt$
