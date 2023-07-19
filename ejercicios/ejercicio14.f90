PROGRAM ejercicio14

!La serie de números enteros de Fibonacci se deine como (Fibonacci-Wikipedia, Fibonnacci-Wolfram):
!Fibn = Fib n−2 + Fibn−1 con n1 = 0, n2 = 1
!Escribir un programa de Fortran que calcule los primeros 12 miembros de la serie de Fibonacci
!resultado: 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89

  IMPLICIT NONE

  !variables
  INTEGER                       ::n1
  
  INTEGER                       ::n2
  
  INTEGER                       ::i
  
  INTEGER, DIMENSION(12)        ::serie_fibonacci
  

  !operaciones
  n1 = 0
  
  n2 = 1
  
  serie_fibonacci(1) = n1
  
  serie_fibonacci(2) = n2
  
  fibonacci: DO i=3,12
  
     serie_fibonacci(i) = serie_fibonacci(i-1) +serie_fibonacci(i-2)
     
  END DO fibonacci

  PRINT *, serie_fibonacci

END PROGRAM ejercicio14
