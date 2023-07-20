PROGRAM ejercicio18_parteII
!DUDA: COMO SE ABRE UN .DAT LINEA POR LINEA??

!lee el archivo ejercicio18.dat y comprueba la conservación de la energia a cada paso de tiempo de la trayectoria

! Etot = Ecin + Epot
! Epot = m * vel**2 / 2
! Ecin = m * g * h 

    IMPLICIT NONE
    
    REAL, DIMENSION(:,:), ALLOCATABLE       :: datos
    INTEGER                  :: i !iterador
    INTEGER                  :: cantidad_filas !cantidad de filas a leer
    REAL                     :: energia
    
    !seteo dimension de matriz de datos
    cantidad_filas = 6
    
    IF (ALLOCATED(datos)) DEALLOCATE(datos)
    ALLOCATE(datos(cantidad_filas,5))
    
    OPEN(UNIT=9, FILE='ejercicio18.dat', STATUS='old')
    DO i = 1, cantidad_filas
       READ(9, 20) datos(i,:)
       PRINT *, "DATOS TIEMPO", i
       PRINT *, datos(i,:) !veo que va cargando de a una fila por vez. Los datos estan dados por: (/tiempo, posicionx_anterior, posiciony_anterior, velocidadx_anterior, velocidady_anterior/)
       !Para cada fila ahora calculo energia: 
       
       energia = (datos(i, 4) ** 2 + datos(i, 4) ** 2) / 2. + 9.81 * datos(i, 3) !considero m = 1
       PRINT *, "La energia total vale:"
       PRINT *, energia
       PRINT *, "!!!!!!!!"
       
    END DO
    CLOSE(9)
    20 FORMAT (6(F7.2,1X)) !5 numeros F enteros de 7 lugares con 2 decimales separados por un espacio
   

END PROGRAM 



