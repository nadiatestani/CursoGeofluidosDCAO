PROGRAM ejercicio3

! Programa para convertir una temperatura de grados Celsius C a grados Faranheit F

IMPLICIT NONE

CHARACTER(len=40)         :: texto_pedido
REAL                    :: temperatura_celcius
REAL                    :: temperatura_far

texto_pedido = "Ingresa la temperatura en grados celcius"

PRINT *, texto_pedido
READ *, temperatura_celcius

temperatura_far = temperatura_celcius* .180/.100 + .32

PRINT *, "La temperatura en Far es:", temperatura_far

END PROGRAM
