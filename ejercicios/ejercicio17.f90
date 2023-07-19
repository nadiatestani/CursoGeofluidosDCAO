PROGRAM ejercicio17
!Usando sólo bucles, construir una matriz 10x10 con los 100 números reales consecutivos (de 1. a 100., rellenado
!por filas) y muestre/calcule:

!(a) Imprimir la matriz (10 filas x 10 columnas con números reales con 2 decimales)
!(b) Diagonal de la matriz
!(c) Valor medio de la matriz
!(d) Desviación estandard de la matriz
!(e) Valor medio de cada columna
!(f) Valor medio de cada fila
!(g) Valor medio de los elemntos pares (media de mat(i, j), i + j = 2n; n ∈ Z)
!(h) La diferencia entre todos los valores por debajo de la diagonal y todos los valores por encima (exceptuando
!la diagonal)

   IMPLICIT NONE
   
   !variables
   INTEGER                               :: i, j !posiciones de la matriz
   REAL, DIMENSION(10,10)                :: matriz !matriz de 10x10
   REAL                                  :: valor_medio !valor medio de matriz
   REAL                                  :: suma_todo !suma de todos los elementos de la matriz
   REAL                                  :: desvio 
   REAL                                  :: suma_desvios
   REAL                                  :: suma_columna
   REAL                                  :: suma_fila
   REAL                                  :: suma_pares
   REAL                                  :: valor_medio_pares
   INTEGER                               :: k !cantidad de elementos pares de matriz
   REAL                                  :: suma_arriba_de_diag
   REAL                                  :: suma_debajo_de_diag
   REAL                                  :: diferencia
   !opero
   
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !completo matriz
   matriz = 0. !primero es una matriz de todos 0. , esto es posible desde fortran90
   DO i=1, 10   !este iterador recorre las filas
      DO j=1, 10   !este iterador recorre las columnas
      
         IF (i == 10 .AND. j == 10) THEN
            matriz(i, j) = 100.
         ELSE
            matriz(i,j) = j + (i-1)*10
         END IF
            
      END DO
      !imprimo matriz
      PRINT *, "MATRIZ:"
      PRINT "(10(F7.2,1X))", (matriz(i,j), j=1, 10) 

   END DO
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !imprimo diagonal de matriz
   PRINT *, " "
   PRINT *, "DIAGONAL DE MATRIZ:"
   PRINT "(10(F7.2,1X))", (matriz(j,j), j=1, 10)
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !imprimo el valor medio de la matriz
   suma_todo = 0.
   
   DO i=1, 10   !este iterador recorre las filas
      DO j=1, 10   !este iterador recorre las columnas      
         suma_todo = suma_todo + matriz(i, j) 
      END DO
   END DO
   
   valor_medio = suma_todo / 100
   PRINT *, " "
   PRINT *, "VALOR MEDIO DE MATRIZ:"
   PRINT "(10(F7.2))", valor_medio
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !imprimo desvio estandar de matriz
   suma_desvios = 0.
   
   DO i=1, 10   !este iterador recorre las filas
      DO j=1, 10   !este iterador recorre las columnas
      
         suma_desvios = suma_desvios + (matriz(i,j) - valor_medio) ** 2

      END DO
   END DO
   
   desvio = (suma_desvios/(100-1)) ** (1./2.)
   PRINT *, " "
   PRINT *, "DESVIO DE MATRIZ:"
   PRINT "(10(F7.2))", desvio
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Imprimo el valor medio de cada columna
   PRINT *, " "
   PRINT *, "MEDIA DE CADA COLUMNA:"
   
   DO j=1, 10   !este iterador recorre las columnas
      suma_columna = 0.
      DO i=1, 10   !este iterador recorre las filas
      
         suma_columna = suma_columna + matriz(i,j) 

      END DO
      !aca ya recorrio la columna 1 y sumo sus valores, ahora calculo la media y la imprimo
      PRINT "(10(F7.2))", suma_columna/10
   END DO
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Imprimo el valor medio de cada fila
   PRINT *, " "
   PRINT *, "MEDIA DE CADA FILA:"
   
   DO i=1, 10   !este iterador recorre las filas
      suma_fila = 0.
      DO j=1, 10   !este iterador recorre las columnas
      
         suma_fila = suma_fila + matriz(i,j) 

      END DO
      !aca ya recorrio la fila 1 y sumo sus valores, ahora calculo la media y la imprimo
      PRINT "(10(F7.2))", suma_fila/10
   END DO
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !imprimo el valor medio de los elemwntos pares de la matriz
   suma_pares = 0.
   k = 0
   DO i=1, 10   !este iterador recorre las filas
      DO j=1, 10   !este iterador recorre las columnas      
         IF (MOD(i+j, 2) == 0) THEN !si el elemento es par
            suma_pares = suma_pares + matriz(i, j) 
            k = k+1
         END IF
      END DO
   END DO
   
   valor_medio_pares = suma_pares / k
   PRINT *, " "
   PRINT *, "VALOR MEDIO DE ELEMENTOS PARES DE MATRIZ:"
   PRINT "(10(F7.2))", valor_medio_pares
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !imprimo diferencia entre elementos de arriba y abajo de diagonal
   diferencia = 0.
   DO i=1, 10   !este iterador recorre las filas
      DO j=1, 10   !este iterador recorre las columnas      
         IF (i < j) THEN !si estoy arriba de diagonal
            diferencia = diferencia + matriz(i, j) 
         ELSE IF (i>j) THEN !si estoy debajo de diagonal
            diferencia = diferencia - matriz(i, j) 
         END IF
      END DO
   END DO
   
   PRINT *, " "
   PRINT *, "DIFERENCIA ARRIBA Y ABAJO DE DIAGONAL:"
   PRINT "(F8.2)", diferencia
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   
   
END PROGRAM ejercicio17


