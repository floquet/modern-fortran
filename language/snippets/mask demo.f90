!  https://gcc.gnu.org/onlinedocs/gcc-4.9.2/gfortran/PRODUCT.html#PRODUCT

PROGRAM test_product
  INTEGER :: x ( 5 ) = [ 1, 2, 3, 4 ,5 ]
  print *, 'x = ', x
  print *, 'PRODUCT ( x ) = ', PRODUCT(x)! all elements, product = 120
  print *, 'PRODUCT ( x, MASK = MOD ( x, 2 ) == 1 ) = ', PRODUCT(x, MASK=MOD(x, 2)==1) ! odd elements, product = 15
END PROGRAM