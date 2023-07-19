PROGRAM ejercicio10_2_funcion 

   IMPLICIT NONE

   REAL                      :: area
   REAL                      ::volumen
   REAL               :: radio_usuarie

   radio_usuarie = 2
   PRINT *, "Area:", area(radio_usuarie)
   PRINT *, "Volumen:", volumen(radio_usuarie)

END PROGRAM ejercicio10_2_funcion 


REAL FUNCTION area(radio_usuarie)
   IMPLICIT NONE
   REAL, INTENT(in)                :: radio_usuarie
   area = 3.14 * radio_usuarie
   RETURN
END FUNCTION area


REAL FUNCTION volumen(radio_usuarie)
   IMPLICIT NONE
   REAL, INTENT(in)                :: radio_usuarie
   volumen = .4/.3 * 3.14 * radio_usuarie
   RETURN
END FUNCTION volumen



