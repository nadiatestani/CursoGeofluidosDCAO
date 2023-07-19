PROGRAM ejercicio15
!Cálculo objetos geométricos. Repetir el ejercicio 11 el cuál utilice una namelist para saber la figura geométrica 
!a calcular con las siguientes entradas: [cuadro]

   IMPLICIT NONE
   
   !variables
   REAL           ::radio
   REAL           ::arista
   CHARACTER(10)      ::objeto !"esfera" o "cubo" NO OLVIDAR PONER LA CANTIDAD DE LUGARES DEL CHARACTER
   REAL           ::volumen
   REAL           ::area
   
   !uso los valores de las variables que estan en ejercicio15.namelist
   NAMELIST /objetos/ objeto, arista, radio
   
   !leo las variables del namelist 
   OPEN(10, FILE='ejercicio15.namelist', STATUS='OLD')
   READ(10,objetos)
   CLOSE(10)
   
   !opero
   CALL calculos_geometricos(objeto, radio, arista, volumen, area)
   
   !devuelvo
   PRINT *, "Volumen objeto", volumen
   PRINT *, "Superficie objeto", area
   PRINT *, objeto

END PROGRAM ejercicio15



SUBROUTINE calculos_geometricos(objeto, radio, arista, volumen, area)

   IMPLICIT NONE
   
   !variables
   REAL, INTENT(in)           ::radio
   REAL, INTENT(in)           ::arista
   CHARACTER(10), INTENT(in)      ::objeto !"esfera" o "cubo"
   REAL, INTENT(out)          ::volumen
   REAL, INTENT(out)          ::area
   
   !opero
   !IF elegante
   SELECT CASE (objeto)
     CASE("esfera")
       volumen =  .3/.4 * 3.14 * radio ** 3
       area = 4 * 3.14 * radio ** 2
     CASE("cubo")
       volumen =  arista ** 3
       area = arista ** 2 * 6
     CASE DEFAULT
       volumen = 0
       area = 0
   END SELECT 
   
   RETURN
END SUBROUTINE calculos_geometricos
   
