!  f90_util.f90
!
!  Installation test program supplied with the NAG Fortran Compiler
!
!  Copyright 1991-2009 The Numerical Algorithms Group Ltd., Oxford, U.K.
!
!  Malcolm Cohen, Robert Iles, July 1991
!
!  Release 3: 1.1, 98/06/23
!
!  Release 4: 1.2, 03/09/15
!
! Release 5.1: 1.1, 04/10/27
!
! Release 5.2: $Id: f90_util.f90 2496 2012-10-16 13:01:05Z ianh $
!
program f90_util

   use f90_iostat
   implicit none
   integer :: i  = 0
   integer :: is = IOERR_OK

1  format(/,1x,a)
2  format(1x,a)
3  format()
   print 1, "   NAG Fortran Compiler"
   print 2, "   ===================="
   print 1, "Installation test and simple demo program"
   print 2, "Copyright 1991-2012 The Numerical Algorithms Group Ltd, Oxford, U.K."

   do  
     print 1, "1 - Binary, octal and hexadecimal constants "
     print 2, "2 - Show maths model (integer and logical)  "
     print 2, "3 - Show maths model (real and complex)     "
     print 2, "4 - Date and time plus system clock         "

     write(*,1,advance='no',iostat=is) "Enter a value (0 to exit): "
     if (is/=IOERR_OK) exit
     read(*,*,iostat=is) i
     if (is/=IOERR_OK) cycle
     print 3
     select case(i) 
     case(0)
       exit
     case(1)
       call demo_1
     case(2)
       call demo_2
     case(3)
       call demo_3
     case(4)
       call demo_4
     end select
   enddo

end program f90_util
!
!  Show the ordering and interpretation of binary, octal and hexadecimal
!  constants
!
subroutine demo_1

   implicit none

   integer :: b1, b2, b3, b4
   integer :: o1, o2, o3, o4
   integer :: z1, z2, z3, z4
 
   data b1, b2, b3, b4 /b'1', b'10', b'100', b'1000'/   
   data o1, o2, o3, o4 /o'7', o'70', o'700', o'7000'/   
   data z1, z2, z3, z4 /z'f', z'f0', z'f00', z'f000'/   

   print '(a,4a6)',   "     Binary: ", "1", "10", "100", "1000"
   print '(a,4b6)',   "             ", b1, b2, b3, b4
   print '(a,4i6,a)', "            (", b1, b2, b3, b4,")"
   print *, " "
   print '(a,4a6)',   "      Octal: ", "7", "70", "700", "7000"
   print '(a,4o6)',   "             ", o1, o2, o3, o4
   print '(a,4i6,a)', "            (", o1, o2, o3, o4,")"
   print *, " "
   print '(a,4a6)',   "Hexadecimal: ", "f", "f0", "f00", "f000"
   print '(a,4z6)',   "             ", z1, z2, z3, z4
   print '(a,4i6,a)', "            (", z1, z2, z3, z4,")"

end subroutine demo_1
!
subroutine demo_2
   use f90_kind

   implicit none

   integer :: i
   integer(INT8) i1
   integer(INT16) i2
   integer(INT32) i3
   integer(INT64) i4

1  format(/,1x,a,a11,a8,a9,a12,a21)
2  format(1x,a,i11,i8,i9,i12,i21)
   print 1,"INTEGER         ", "Default", "int8", "int16", "int32","int64"
   print 2,"  KIND number = ",kind(1),int8,int16,int32,int64
   print 2,"       digits = ",digits(i),digits(i1),digits(i2),digits(i3),digits(i4)
   print 2,"        radix = ",radix(i),radix(i1),radix(i2),radix(i3),radix(i4)
   print 2,"        range = ",range(i),range(i1),range(i2),range(i3),range(i4)
   print 2,"         huge = ",huge(i),huge(i1),huge(i2),huge(i3),huge(i4)
   print 2,"     bit_size = ",bit_size(i),bit_size(i1),bit_size(i2), &
                              bit_size(i3),bit_size(i4)
   print 1,"LOGICAL         ", "Default", "byte", "word","twobyte", "logical64"
   print 2,"  KIND number = ",kind(.true.),byte,twobyte,word,logical64

end subroutine demo_2
!
subroutine demo_3
   use f90_kind

   implicit none
   integer, parameter :: qp = max(selected_real_kind(30),kind(0.0))
   real :: j
   complex :: k 
   real(single) s
   complex(single) c
   real(double) d
   complex(double) dc
   real(qp) q
   complex(qp) qc

1  format(/,1x,a,3a15)
2  format(1x,a,3i15)
3  format(1x,a,3es15.8)
4  format(/,1x,a,4a15)
5  format(1x,a,4i15)
6  format(1x,a,4es15.6e4)

   if (kind(q)/=kind(0.0)) then
      ! We have Quad precision
      print 4,"REAL            ", "Default", "single", "double", "quad"
      print 5,"  KIND number = ",kind(1.0),single, double, qp
      print 5,"       digits = ",digits(j),digits(s),digits(d),digits(q)
      print 5,"  maxexponent = ",maxexponent(j),maxexponent(s),maxexponent(d),&
           maxexponent(q)
      print 5,"  minexponent = ",minexponent(j),minexponent(s),minexponent(d),&
           minexponent(q)
      print 5,"    precision = ",precision(j),precision(s),precision(d),&
           precision(q)
      print 5,"        radix = ",radix(j),radix(s),radix(d),radix(q)
      print 5,"        range = ",range(j),range(s),range(d),range(q)
      print 6,"      epsilon = ",epsilon(j),epsilon(s),epsilon(d),epsilon(q)
      print 6,"         tiny = ",tiny(j),tiny(s),tiny(d),tiny(q)
      print 6,"         huge = ",huge(j),huge(s),huge(d),huge(q)
      
      print 4,"COMPLEX         ", "Default", "single", "double", "quad"
      print 5,"  KIND number = ",kind(k),single, double, qp
      print 5,"    precision = ",precision(k),precision(c),precision(dc), &
           precision(qc)
      print 5,"        range = ",range(k),range(c),range(dc),range(qc)
   else
      print 1,"REAL            ", "Default", "single", "double"
      print 2,"  KIND number = ",kind(1.0),single, double
      print 2,"       digits = ",digits(j),digits(s),digits(d)
      print 2,"  maxexponent = ",maxexponent(j),maxexponent(s),maxexponent(d)
      print 2,"  minexponent = ",minexponent(j),minexponent(s),minexponent(d)
      print 2,"    precision = ",precision(j),precision(s),precision(d)
      print 2,"        radix = ",radix(j),radix(s),radix(d)
      print 2,"        range = ",range(j),range(s),range(d)
      print 3,"      epsilon = ",epsilon(j),epsilon(s),epsilon(d)
      print 3,"         tiny = ",tiny(j),tiny(s),tiny(d)
      print 3,"         huge = ",huge(j),huge(s),huge(d)
      
      print 1,"COMPLEX         ", "Default", "single", "double"
      print 2,"  KIND number = ",kind(k),single, double
      print 2,"    precision = ",precision(k),precision(c),precision(dc)
      print 2,"        range = ",range(k),range(c),range(dc)
   endif
end subroutine demo_3
!
subroutine demo_4

  implicit none
  integer :: count, rate, max
  character(len= 8) :: date
  character(len=10) :: time
  character(len= 5) :: zone
  integer :: values(8)

  call date_and_time(date=date, time=time, zone=zone, values=values)
  print *,'DATE_AND_TIME'
  print *,'     date: ', date
  print *,'     time: ', time
  print *,'     zone: ', zone
  print *,'   values: ', values

  print *,''

  call system_clock(count_max=max, count_rate=rate, count=count)
  print *,'SYSTEM_CLOCK'
  print *,'        count:', count
  Write (*,'(1x,a,1x,i0,", i.e. approx every ")',advance='no') &
    '   count_rate:', rate
  If (rate>1000000) Then
    Write (*,'(f0.1,1x,a)') 1d9/rate,'nanoseconds'
  Else If (rate>1000) Then
    Write (*,'(f0.1,1x,a)') 1d6/rate,'microseconds'
  Else
    Write (*,'(f0.1,1x,a)') 1d3/rate,'milliseconds'
  End If
  Write (*,'(1x,a,1x,i0)',advance='no') '    count_max:', max
  If ((max+1d0)/rate<60**2) Then
    Write (*,"(' (',f0.1,' minutes)')") ((max+1d0)/rate)/60
  Else
    Write (*,"(' (',f0.2,' hours)')") ((max+1d0)/rate)/(60**2)
  End If

end subroutine demo_4