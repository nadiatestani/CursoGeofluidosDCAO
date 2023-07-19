PROGRAM ejercicio10_4_funcion

IMPLICIT NONE

REAL                 ::calculo_volumen
REAL                 ::arista
REAL                 ::vol

arista = 2
vol = calculo_volumen(arista)
PRINT *, vol

END PROGRAM

REAL FUNCTION calculo_volumen(arista)

REAL, INTENT(in)     ::arista

calculo_volumen = arista**3
RETURN

END FUNCTION
