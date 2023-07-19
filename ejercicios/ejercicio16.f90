PROGRAM ejercicio16
! Calcula la serie DCAO que se rige por la siguiente norma (tomando las letras de las siglas y con vocales: +, consonantes: -):
! dcao n = −dcao n−4 − dcao n−3 + dcao n−2 + dcao n−1
! con n 1 = 0, n 2 = 1, n 3 = 0, n 4 = 1

!El programa pide cuantos números se quiren de la serie DCAO de números. 
!El programa usa un vector para almacenar los 4 números previos necesarios para calcular el número DCAO.
!El programa utiliza una subroutina para avanzar los valores de este vector con el miembro DCAO más reciente, para calcular el miembro siguiente-
!ALLOCATE tiene que estar fuera de la subrutina

   IMPLICIT NONE
   
   !variables
   REAL, DIMENSION(:), ALLOCATABLE                      ::vector_serie !el tamaño del vector se puede modificar en el programa 
   REAL, DIMENSION(4)                                   ::vector_de_cuatro !aca se almacenan los ultimos 4 numeros de vector_serie
   INTEGER                                              ::cantidad_numeros_en_serie
   INTEGER                                              ::i !iterador
   REAL                                                 ::ultimo_miembro_serie

   cantidad_numeros_en_serie = 10
   
   IF (ALLOCATED(vector_serie)) DEALLOCATE(vector_serie)
   ALLOCATE(vector_serie(cantidad_numeros_en_serie))
   
   vector_de_cuatro(1:4) = (/0, 1, 0, 1/)
   vector_serie = vector_de_cuatro
   
   !esto lo dejo porque funcionaba sin la subrutina: 
   !vector_serie(1:4) = (/0, 1, 0, 1/)
   !genero_serie: DO i = 5, cantidad_numeros_en_serie
   !   vector_serie(i) = - vector_serie(i-4) - vector_serie(i-3) + vector_serie(i-2) + vector_serie(i-1)
   !END DO genero_serie
   
   genero_serie: DO i=5, cantidad_numeros_en_serie
       ultimo_miembro_serie = -vector_de_cuatro(i-4) - vector_de_cuatro(i-3) + vector_de_cuatro(i-2) + vector_de_cuatro(i-1) !genero nuevo numero para serie
       vector_serie(i) = ultimo_miembro_serie                                                                                !pongo ese valor en la posicion i del vector_serie
       CALL actualizo_vector_de_cuatro(vector_de_cuatro, ultimo_miembro_serie)      
       
   END DO genero_serie
   PRINT *, vector_serie

END PROGRAM ejercicio16


SUBROUTINE actualizo_vector_de_cuatro(vector_de_cuatro, ultimo_miembro_serie)
   !actualiza el vector_de_cuatro con el ultimo_miembro_serie
   IMPLICIT NONE
   
   !variables
   REAL, DIMENSION(4), INTENT(inout)                                 ::vector_de_cuatro
   REAL, INTENT(in)                                                  ::ultimo_miembro_serie
   
   !opero
   vector_de_cuatro(1:4) = (/vector_de_cuatro(2), vector_de_cuatro(3), vector_de_cuatro(4), ultimo_miembro_serie/)
   
   RETURN
   
END SUBROUTINE 
