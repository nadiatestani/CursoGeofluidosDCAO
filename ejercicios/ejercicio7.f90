PROGRAM ejercicio7

! Calcular un factorial. Comprobar la diferencia entre el máximo número posible permitido si se utilizan valores enteros o reales 
! N ! = N × (N − 1) × (N − 2) × ...1

!Este programa usa ciclos. esta medio mal el calculo pero la idea esta
! con enteros es mejor, tiene mas memoria y puedo calcular el factorial de numeros mas grandes


IMPLICIT NONE

INTEGER                       ::i, n
REAL                          ::n_factorial

n = 3
n_factorial = 3

factoriando: DO i=1, n
n_factorial = n_factorial * (n-1)

END DO factoriando

PRINT *,n_factorial


END PROGRAM

