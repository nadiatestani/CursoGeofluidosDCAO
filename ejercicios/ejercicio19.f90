PROGRAM ejercicio19

!tengo que poder calcular a priori la cantidad de casos que verifican la condicion para poder armar la matriz con las coordenadas ???? 

!Devuelve las coordenadas de los valores de una matriz que cumplan cierto
!criterio. Tareas del programa

!(a) Crear una matrix de enteros de 5x5 rellenada por filas
!(b) Crear una subroutina que devuelva en forma de vector con dimensiones (Npuntos,2) las coordenadas de los
!valores que sean pares. La misma subroutina que devuelva el numero de puntos que cumplan la condicion.
!Dicha subroutina contendra dos parámetros:
   ! [condicion]: Una cadena de texto para determinar el criterio que se tiene que cumplir. Utilizar una estructura CASE para la selección
   ! [valor]: Un numero a seguir en función del criterio
   ! (tabla con criterios en pdf de ejercicios)
!(c) En caso que la condición sea igual, mayor, menor el programa tiene que pedir el [valor] a buscar
!(d) Imprimir por pantalla el numero de puntos y las coordenadas encontradas

   IMPLICIT NONE
   
   !variables
   INTEGER, DIMENSION(5,5)               :: matriz !matriz de numeros enteros de 5X5
   INTEGER                               :: i, j !iteradores sobre i filas j columnas
   CHARACTER(20)                             :: condicion
   REAL                                  :: valor !para condicion
   INTEGER, DIMENSION(25, 2)             :: coordenadas !de matriz que verifican condicion
   INTEGER                                     :: k !cantidad de casos, sale de subrutina
   INTEGER                               :: m ! iterador para impresion
   
   !a) creo matriz de enteros de 5x5 rellenada por filas
   matriz = 0.
   DO i=1, 5
     DO j=1, 5
       matriz(i,j) = INT(i*j)
     END DO
   END DO
   
   !b c d
   !condicion igual
   condicion = 'igual'
   valor = 5
   CALL chequeo_condicion(matriz, condicion, valor, coordenadas, k)
   
   PRINT *, "VALOR =", valor
   PRINT *, "---------------------"
   PRINT *, "CONDICION"
   PRINT *, condicion
   PRINT *, "CANTIDAD DE CASOS"
   PRINT *, K
   DO m = 1, k !cantidad_de_casos tiene que salir de chequeo_condicion
      PRINT *, m, ":", matriz(coordenadas(m,1),coordenadas(m,2)), "=", coordenadas(m,:)
   END DO

   !condicion igual
   condicion = 'igual'
   valor = 5
   CALL chequeo_condicion(matriz, condicion, valor, coordenadas, k)
   
   PRINT *, "---------------------"
   PRINT *, "CONDICION"
   PRINT *, condicion
   PRINT *, "CANTIDAD DE CASOS"
   PRINT *, K
   DO m = 1, k !cantidad_de_casos tiene que salir de chequeo_condicion
      PRINT *, m, ":", matriz(coordenadas(m,1),coordenadas(m,2)), "=", coordenadas(m,:)
   END DO
   
   !condicion mayor
   condicion = 'mayor'
   valor = 5
   CALL chequeo_condicion(matriz, condicion, valor, coordenadas, k)
   
   PRINT *, "---------------------"
   PRINT *, "CONDICION"
   PRINT *, condicion
   PRINT *, "CANTIDAD DE CASOS"
   PRINT *, K
   DO m = 1, k !cantidad_de_casos tiene que salir de chequeo_condicion
      PRINT *, m, ":", matriz(coordenadas(m,1),coordenadas(m,2)), "=", coordenadas(m,:)
   END DO
   
   !condicion menor
   condicion = 'menor'
   valor = 5
   CALL chequeo_condicion(matriz, condicion, valor, coordenadas, k)
   
   PRINT *, "---------------------"
   PRINT *, "CONDICION"
   PRINT *, condicion
   PRINT *, "CANTIDAD DE CASOS"
   PRINT *, K
   DO m = 1, k !cantidad_de_casos tiene que salir de chequeo_condicion
      PRINT *, m, ":", matriz(coordenadas(m,1),coordenadas(m,2)), "=", coordenadas(m,:)
   END DO
   
   !condicion par
   condicion = 'par'
   valor = 5
   CALL chequeo_condicion(matriz, condicion, valor, coordenadas, k)
   
   PRINT *, "---------------------"
   PRINT *, "CONDICION"
   PRINT *, condicion
   PRINT *, "CANTIDAD DE CASOS"
   PRINT *, K
   DO m = 1, k !cantidad_de_casos tiene que salir de chequeo_condicion
      PRINT *, m, ":", matriz(coordenadas(m,1),coordenadas(m,2)), "=", coordenadas(m,:)
   END DO
   
   !condicion impar
   condicion = 'impar'
   valor = 5
   CALL chequeo_condicion(matriz, condicion, valor, coordenadas, k)
   
   PRINT *, "---------------------"
   PRINT *, "CONDICION"
   PRINT *, condicion
   PRINT *, "CANTIDAD DE CASOS"
   PRINT *, K
   DO m = 1, k !cantidad_de_casos tiene que salir de chequeo_condicion
      PRINT *, m, ":", matriz(coordenadas(m,1),coordenadas(m,2)), "=", coordenadas(m,:)
   END DO

END PROGRAM 


SUBROUTINE chequeo_condicion(matriz, condicion, valor, coordenadas, k)
   ! entra una matriz de datos, el nombre de una condicion, el valor que necesite esta condicion xej: mayor a [valor]
   ! salen las coordenadas de la matriz en la que eso se verifica
   
   IMPLICIT NONE
   
   INTEGER, DIMENSION(5,5), INTENT(in)         :: matriz
   INTEGER                                     :: i, j !iteradores sobre i filas j columnas
   CHARACTER(20), INTENT(IN)                       :: condicion
   REAL, INTENT(IN)                            :: valor !para condicion
   INTEGER, DIMENSION(25, 2), INTENT(out)                       :: coordenadas !de matriz que verifican condicion 
   INTEGER, INTENT(out)                                     :: k !cantidad de casos que verifican
   
   PRINT *, condicion
   
   SELECT CASE (TRIM(condicion))
     CASE ('igual')
         k =1 
         DO i = 1,5
            DO j = 1,5
               IF (matriz(i,j) == valor) THEN 
                   coordenadas(k, :) = (/i, j/)
                   k = k+1                              
                END IF
             END DO
         END DO
         k = k-1 
      
      CASE ('mayor')
         k =1 
         DO i = 1,5
            DO j = 1,5
               IF (matriz(i,j) > valor) THEN 
                   coordenadas(k, :) = (/i, j/)
                   k = k+1                              
                END IF
             END DO
         END DO
         k = k-1 
      
      CASE ('menor')
         k =1 
         DO i = 1,5
            DO j = 1,5
               IF (matriz(i,j) < valor) THEN 
                   coordenadas(k, :) = (/i, j/)
                   k = k+1                              
                END IF
             END DO
         END DO
         k = k-1 
      
      CASE ('par')
         k =1 
         DO i = 1,5
            DO j = 1,5
               IF (MOD(matriz(i,j),2) == 0) THEN   
                   coordenadas(k, :) = (/i, j/)
                   k = k+1                              
                END IF
             END DO
         END DO
         k = k-1 
         
      CASE ('impar')
         k =1 
         DO i = 1,5
            DO j = 1,5
               IF (MOD(matriz(i,j),2) /= 0) THEN   
                   coordenadas(k, :) = (/i, j/)
                   k = k+1                              
                END IF
             END DO
         END DO
         k = k-1 
         
      CASE DEFAULT   
         PRINT *, "Ingrese una condicion valida"
      END SELECT
      
   
   RETURN
END SUBROUTINE
