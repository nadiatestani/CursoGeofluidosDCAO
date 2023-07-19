PROGRAM ejercicio1

! Programa que pida tu nombre y escriba por pantalla Bienvenido a Fortran '[nombre]' !!

IMPLICIT NONE !siempre escribir esto

CHARACTER(len=20)         :: texto_pedido
CHARACTER(len=10)         :: texto_usuarie

texto_pedido = "Me decis tu nombre?"

PRINT *, texto_pedido

READ *, texto_usuarie

PRINT *,"Bienvenide!!", texto_usuarie

END PROGRAM ejercicio1


