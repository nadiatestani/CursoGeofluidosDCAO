PROGRAM ejercicio20_reales
!Lee un fichero con una matrix 2D y calcula su valor mínimo y el máximo.
!Tareas del programa
!(a) Pedir el nombre del fichero por pantalla
!(b) Pedir las dimensiones de la matriz
!(c) Escribir una subroutina que a partir del nombre del fichero y las dimensiones devuelva la matriz de datos.
!Hacer una versión para una matriz real y otra de enteros
!(d) Devolver por pantalla el valor mínimo y el máximo

    IMPLICIT NONE
    
    !variables
    CHARACTER(30)                                 ::nombre_fichero
    INTEGER, DIMENSION(:,:), ALLOCATABLE          ::matriz
    !para ingresar argumentos numericos desde pantalla: para las dimensiones
    INTEGER                                       :: iarg, narg
    CHARACTER(LEN=500), ALLOCATABLE, DIMENSION(:) :: dimensiones !dimensiones de matriz, entran por pantalla asi: ./ejercicio20_reales 5 5
    INTEGER :: dim1
    INTEGER :: dim2
    
    PRINT *, "Ingrese el nombre del fichero"
    READ *, nombre_fichero
    
    narg=COMMAND_ARGUMENT_COUNT()
    IF (ALLOCATED(dimensiones)) DEALLOCATE(dimensiones)
    ALLOCATE(dimensiones(narg))
    ! Agarrar argumentos
    DO iarg=1, narg
       CALL GETARG(iarg,dimensiones(iarg))
    END DO
    ! Convertir a tipo los argumentos
    READ(dimensiones(1), '(I10)')dim1
    READ(dimensiones(2), '(I10)')dim2
    DEALLOCATE(dimensiones)
    
    !CALL abrir_matriz_de_datos(nombre_fichero, dim1, dim2, matriz)
    OPEN(UNIT=9, FILE=nombre_fichero, STATUS='old')
    DO i = 1, dim1
       READ(9, *) (matriz(i,j),j=1,dim2)     ! j va desde 1 hasta dim2 
    END DO
    CLOSE(9)
    
END PROGRAM 



!SUBROUTINE abrir_matriz_de_datos(nombre_fichero, dim1, dim2, matriz)

!    IMPLICIT NONE
    
!    CHARACTER(30), INTENT(in)                       ::nombre_fichero
!    INTEGER, INTENT(in)                             ::dim1, dim2
!    INTEGER, DIMENSION(dim1, dim2), INTENT(out)             ::matriz 
!    INTEGER                                         ::i,j

!    OPEN(UNIT=9, FILE=nombre_fichero, STATUS='old')
!    DO i = 1, dim1
!       READ(9, *) (matriz(i,j),j=1,dim2)     ! j va desde 1 hasta dim2 
!    END DO
!    CLOSE(9)
   
!END SUBROUTINE




