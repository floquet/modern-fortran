c         Co-Array FORTRAN.
c
c
      implicit real * 8 (a-h, o-z)
c
      include 'mpif.h'
c
      parameter (n = 100, np1 = n + 1, iband = 10)
      common / sh / a(n, n), b(n), row(2 * n)
      dimension buff(n * 2)[*]
c
c         Initialize MPI.
c
      tag = 100
      call MPI_INIT (ierror)
      call MPI_COMM_RANK (MPI_COMM_WORLD, myid, ierror)
      call MPI_COMM_SIZE (MPI_COMM_WORLD, noproc, ierror)
c
      mpi_caf = 2
c
c         Set constants.
c
      nm1 = n - 1
      no2 = n / 2
      nmband = n - iband
      nmbp1 = nmband + 1
c
c         Compute the system of equations.
c
      do j = 1, n
        do i = 1, n
          a(i, j) = 0.
        end do
        a(j, j) = 6.
      end do
c
      do i = 1, nm1
        a(i, i + 1) = -1.
        a(i + 1, i) = -1.
      end do
c
      do i = 1, nmband
        a(i, i + iband) = -1.
        a(i + iband, i) = -1.
      end do
c
      do i = 1, n
        if ((i .eq. 1) .or. (i .eq. n)) then
          b(i) = 4.
        else if ((i .le. iband) .or. (i .ge. nmbp1)) then
          b(i) = 3.
        else
          b(i) = 2.
        end if
      end do
c
c         Define ownership of rows.
c
      ichunk = n / noproc
      left = mod (n, noproc)
      if (left .ne. 0) ichunk = ichunk + 1
      irow1 = myid * ichunk + 1
      irow2 = min0 (irow1 + ichunk - 1, n)
c
c         Do Gauss Elimination.
c
      do k = 1, nm1
c
        kp1 = k + 1
c
c         Send and receive data.
c
        if (mpi_caf .eq. 2) then
          sync all
        end if
c
        iroot = (k - 1) / ichunk
c
        if (myid .eq. iroot) then
c
c         Collect the temporary row.
c
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
c
        end if
c
        if (mpi_caf .eq. 2) then
          sync all
        end if
c
c         Broadcast to other processors.
c
        if (mpi_caf .eq. 1) then
          call MPI_BCAST (row(k), np1 - k + 1, MPI_REAL8, iroot,
     &      MPI_COMM_WORLD, ierror)
        else
          row(k:np1) = buff(k:np1)[iroot + 1]
        end if
c
c         Do the rows.
c
        istart = max0 (kp1, irow1)
        iend = irow2
c
c         Do the work now.
c
        if (istart .le. iend) then
c
          aa = 1.0d0 / row(k)
          bb = row(np1) * aa
c
          do j = kp1, n
            do i = istart, iend
              a(i, j) = a(i, j) - a(i, k) * row(j) * aa
            end do
          end do
c
          do i = istart, iend
            b(i) = b(i) - a(i, k) * bb
          end do
c
        end if
c
      end do
c
      if (mpi_caf .eq. 1) then
        call MPI_BARRIER (MPI_COMM_WORLD, ierror)
      else
        sync all
      end if
c
c         Do back substitution.
c
      do jj = 1, n
c
        j = n - jj
        jp1 = j + 1
c
c         Send and receive data.
c
        iroot = (jp1 - 1) / ichunk
c
        if (myid .eq. iroot) then
          x = b(jp1) / a(jp1, jp1)
          b(jp1) = x
          buff(1) = x
        end if
c
c         Broadcast to other processors.
c
        if (mpi_caf .eq. 1) then
          call MPI_BCAST (x, 1, MPI_REAL8, iroot,
     &      MPI_COMM_WORLD, ierror)
        else
          sync all
          x = buff(1)[iroot + 1]
        end if
c
c         Do the rows.
c
        istart = irow1
        iend =  min0 (irow2, j)
c
c         Do the work now.
c
        if (istart .le. iend) then
          do i = istart, iend
            b(i) = b(i) - a(i, jp1) * x
          end do
        end if
c
      end do
c
c         Collect data.
c
      if (mpi_caf .eq. 1) then
        call MPI_GATHER (b(irow1), ichunk, MPI_REAL8,
     &    buff, ichunk, MPI_REAL8, 0, MPI_COMM_WORLD, ierror)
      else
        buff(irow1:irow2)[1] = b(irow1:irow2)
        sync all
      end if
c
c         Print results.
c
      if (myid .eq. 0) then
c
        do i = 1, n
          b(i) = buff(i)
        end do
c
        write (*, '(5(i5, f10.2))') (i, b(i), i = 1, 10)
        write (*, '(i5, f10.2)') no2, b(no2)
        write (*, '(5(i5, f10.2))') (i, b(i), i = n - 9, n)
c
        print*, 'noproc =', noproc
c
      end if
c
      call MPI_FINALIZE (ierror)
c
      end

