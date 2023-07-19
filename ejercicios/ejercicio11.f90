PROGRAM ejercicio11
! Mediante una subrutina, preparar un programa el cuál calcule el volúmen y la superfície de distintos cuerpos
! geométricos: cubo, esfera y tetraedro regular

   IMPLICIT NONE

   !variables
   REAL                :: arista_o_radio
   REAL               :: vol_cubo
   REAL               :: vol_esfera
   REAL               :: area_cubo
   REAL               :: area_esfera
      
   !opero
   arista_o_radio = 2
   CALL calculos_vol_sup(arista_o_radio, vol_cubo, vol_esfera, area_cubo, area_esfera)
  
  !devuelvo
   PRINT *, "Vol_cubo:", vol_cubo 
   PRINT *, "Vol_esfera:", vol_esfera 
   !... etc
END PROGRAM

SUBROUTINE calculos_vol_sup(arista_o_radio, vol_cubo, vol_esfera, area_cubo, area_esfera)

   IMPLICIT NONE
   
   !variables
   REAL, INTENT(in)                :: arista_o_radio
   REAL, INTENT(out)               :: vol_cubo
   REAL, INTENT(out)               :: vol_esfera
   REAL, INTENT(out)               :: area_cubo
   REAL, INTENT(out)               :: area_esfera
   
   !opero
   vol_cubo = arista_o_radio **3
   vol_esfera = .3/.4 * 3.14 * arista_o_radio** 3
   area_cubo = arista_o_radio ** 2 *6
   area_esfera = 4 * 3.14 * arista_o_radio**2
   
   RETURN
END SUBROUTINE
   
