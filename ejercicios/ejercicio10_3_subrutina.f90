PROGRAM ejercicio10_3_subrutina
! Programa para convertir una temperatura de grados Celsius C a grados Faranheit F

IMPLICIT NONE

REAL              :: temperatura_celcius
REAL              :: temperatura_far

temperatura_celcius = 32
CALL pasaje_a_radianes(temperatura_celcius, temperatura_far)
PRINT *, temperatura_far

END PROGRAM ejercicio10_3_subrutina

SUBROUTINE pasaje_a_radianes(temperatura_celcius, temperatura_far)

   IMPLICIT NONE

   REAL, INTENT(in)               :: temperatura_celcius
   REAL, INTENT(out)              :: temperatura_far

   temperatura_far = temperatura_celcius* .180/.100 + .32

RETURN
END SUBROUTINE

