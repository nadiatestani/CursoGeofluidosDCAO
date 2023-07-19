PROGRAM ejercicio2

! Programa para calcular area y volumen de un a circunferencia despues de pedir su radio por terminal

IMPLICIT NONE

CHARACTER(len=40)         :: texto_pedido
REAL                      :: radio_usuarie
REAL                      :: volumen
REAL                      :: area



texto_pedido = "Dame el radio de la circunferencia"

PRINT *, texto_pedido
READ *, radio_usuarie

volumen = .4/.3 * 3.14 * radio_usuarie
area = 3.14 * radio_usuarie

PRINT *, "El volumen es", volumen
PRINT *, "El area es", area


END PROGRAM
