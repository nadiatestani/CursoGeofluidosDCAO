PROGRAM ejercicio10_2
!Transformar todos los ejercicios anteriores 2 a 9 en funciones o subrutinas

IMPLICIT NONE

REAL               :: radio_usuarie
REAL                    :: volumen
REAL                   :: area

radio_usuarie = 2

CALL calculos_circunferencia(radio_usuarie,volumen,area)
PRINT *, volumen, area

END PROGRAM ejercicio10_2

SUBROUTINE calculos_circunferencia(radio_usuarie,volumen,area)
!Dentro de la subrutina, se tienen que volver a definir las variables

IMPLICIT NONE

REAL, INTENT(in)                :: radio_usuarie
REAL, INTENT(out)                       :: volumen
REAL, INTENT(out)                     :: area


volumen = .4/.3 * 3.14 * radio_usuarie
area = 3.14 * radio_usuarie

RETURN
END SUBROUTINE calculos_circunferencia


