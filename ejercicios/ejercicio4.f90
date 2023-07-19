PROGRAM ejercicio4

! Leida la longitud de una arista, calcular el volumen del cubo

IMPLICIT NONE

CHARACTER(len=40)         :: texto_pedido
REAL                    :: arista
REAL                    :: volumen

texto_pedido = "Ingresa la longitud de una arista"

PRINT *, texto_pedido
READ *, arista

volumen = arista**3

PRINT *, "El volumen del cubo es", volumen


END PROGRAM
