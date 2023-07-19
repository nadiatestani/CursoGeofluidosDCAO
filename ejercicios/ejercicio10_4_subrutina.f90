PROGRAM ejercicio10_4_subrutina

IMPLICIT NONE

REAL                    :: arista
REAL                    :: volumen

arista = 2
CALL calculo_volumen(arista, volumen)
PRINT *, volumen

END PROGRAM

SUBROUTINE calculo_volumen(arista, volumen)
! Leida la longitud de una arista, calcular el volumen del cubo

IMPLICIT NONE

REAL, INTENT(in)                  :: arista
REAL, INTENT(out)            :: volumen

volumen = arista**3
RETURN
END SUBROUTINE




