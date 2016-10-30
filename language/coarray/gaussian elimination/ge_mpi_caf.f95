!           Co-Array FORTRAN: Fred Tracy
!           Compare MPI and Coarray Fortran
!
      implicit none
!
      use iso_fortran_env

      include 'mpif.h'
!
      integer ( kind = int_64 ), parameter :: wp = selected_real_kind ( 1.0_real64 )
      integer ( kind = int_64 ), parameter :: n = 100, np1 = n + 1, iband = 10

      real ( wp ) :: zero = 0.0_wp, one = 1.0_wp

      common / sh / a(n, n), b(n), row(2 * n)

      dimension buff ( n * 2 ) [ * ]  ! coarray dimensin
!
!         Initialize MPI.
!
      !tag = 100
      call MPI_INIT (ierror)
      call MPI_COMM_RANK (MPI_COMM_WORLD, myid, ierror)
      call MPI_COMM_SIZE (MPI_COMM_WORLD, noproc, ierror)
!
      mpi_caf = 2
!
!         Set constants.
!
      nm1 = n - 1
      no2 = n / 2
      nmband = n - iband
      nmbp1 = nmband + 1
!
!         Compute the system of equations.
!
      a = zero
      ( a ( j, j ) = 6 * one, j = 1, n )
      do j = 1, n
        do i = 1, n
          a(i, j) = 0.
        end do
        a(j, j) = 6.
      end do
!
      do i = 1, nm1
        a(i, i + 1) = -1.
        a(i + 1, i) = -1.
      end do
!
      a ( 1, nmband + 1 ) = -1.
      do i = 1, nmband
        a(i, i + iband) = -1.
        a(i + iband, i) = -1.
      end do
!
      do i = 1, n
        if ((i .eq. 1) .or. (i .eq. n)) then
          b(i) = 4.
        else if ((i .le. iband) .or. (i .ge. nmbp1)) then
          b(i) = 3.
        else
          b(i) = 2.
        end if
      end do
!
!         Define ownership of rows.
!
      ichunk = n / nopro!
      left = mod (n, noproc)
      if (left .ne. 0) ichunk = ichunk + 1
      irow1 = myid * ichunk + 1
      irow2 = min0 (irow1 + ichunk - 1, n)
!
!         Do Gauss Elimination.
!
      do k = 1, nm1
!
        kp1 = k + 1
!
!         Send and receive data.
!
        if (mpi_caf .eq. 2) then
          sync all
        end if
!
        iroot = (k - 1) / ichunk
!
        if (myid .eq. iroot) then
!
!         Collect the temporary row.
!
          if (mpi_caf .eq. 1) then
            do j = k, n
              row(j) = a(k, j)
            end do
            row(np1) = b(k)
          else
            do j = k, n
              buff(j) = a(k, j)
            end do
            buff(np1) = b(k)
          end if
!
        end if
!
        if (mpi_caf .eq. 2) then
          sync all
        end if
!
!         Broadcast to other processors.
!
        if (mpi_caf .eq. 1) then
          call MPI_BCAST (row(k), np1 - k + 1, MPI_REAL8, iroot,
     &      MPI_COMM_WORLD, ierror)
        else
          row(k:np1) = buff(k:np1)[iroot + 1]
        end if
!
!         Do the rows.
!
        istart = max0 (kp1, irow1)
        iend = irow2
!
!         Do the work now.
!
        if (istart .le. iend) then
!
          aa = 1.0d0 / row(k)
          bb = row(np1) * aa
!
          do j = kp1, n
            do i = istart, iend
              a(i, j) = a(i, j) - a(i, k) * row(j) * aa
            end do
          end do
!
          do i = istart, iend
            b(i) = b(i) - a(i, k) * bb
          end do
!
        end if
!
      end do
!
      if (mpi_caf .eq. 1) then
        call MPI_BARRIER (MPI_COMM_WORLD, ierror)
      else
        sync all
      end if
!
!         Do back substitution.
!
      do jj = 1, n
!
        j = n - jj
        jp1 = j + 1
!
!         Send and receive data.
!
        iroot = (jp1 - 1) / ichunk
!
        if (myid .eq. iroot) then
          x = b(jp1) / a(jp1, jp1)
          b(jp1) = x
          buff(1) = x
        end if
!
!         Broadcast to other processors.
!
        if (mpi_caf .eq. 1) then
          call MPI_BCAST (x, 1, MPI_REAL8, iroot,
     &      MPI_COMM_WORLD, ierror)
        else
          sync all
          x = buff(1)[iroot + 1]
        end if
!
!         Do the rows.
!
        istart = irow1
        iend =  min0 (irow2, j)
!
!         Do the work now.
!
        if (istart .le. iend) then
          do i = istart, iend
            b(i) = b(i) - a(i, jp1) * x
          end do
        end if
!
      end do
!
!         Collect data.
!
      if (mpi_caf .eq. 1) then
        call MPI_GATHER (b(irow1), ichunk, MPI_REAL8,
     &    buff, ichunk, MPI_REAL8, 0, MPI_COMM_WORLD, ierror)
      else
        buff(irow1:irow2)[1] = b(irow1:irow2)
        sync all
      end if
!
!         Print results.
!
      if (myid .eq. 0) then
!
        do i = 1, n
          b(i) = buff(i)
        end do
!
        write (*, '(5(i5, f10.2))') (i, b(i), i = 1, 10)
        write (*, '(i5, f10.2)') no2, b(no2)
        write (*, '(5(i5, f10.2))') (i, b(i), i = n - 9, n)
!
        print*, 'noproc =', nopro!
!
      end if
!
      call MPI_FINALIZE (ierror)
!
      end

