PROGRAM ejercicio10_3_funcion

IMPLICIT NONE

REAL              ::celcius_a_far             !defino la variable funcion
REAL              ::temperatura_celcius       !defino la variable de entrada a la funcion

temperatura_celcius = 32

PRINT *, celcius_a_far(temperatura_celcius) !llamo a la funcion

END PROGRAM ejercicio10_3_funcion




REAL FUNCTION celcius_a_far(temperatura_celcius) !defino funcion real llamada celcius_a_far y que tiene como input la temperatura_celcius
!las funciones solo tienen una salida que se debe llamar como el nombre de la funcion

REAL, INTENT(in)              ::temperatura_celcius !defino la variable de entrada

celcius_a_far = temperatura_celcius* .180/.100 + .32 !calculo el output, SE TIENE QUE LLAMAR COMO EL NOMBRE DE LA FUNCION

RETURN
END FUNCTION 
