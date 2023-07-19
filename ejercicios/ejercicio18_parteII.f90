PROGRAM ejercicio18_parteII
!DUDA: COMO SE ABRE UN .DAT LINEA POR LINEA??

!lee el archivo ejercicio18.dat y comprueba la conservación de la energia a cada paso de tiempo de la trayectoria

! Etot = Ecin + Epot
! Epot = m * vel**2 / 2
! Ecin = m * g * h 

    OPEN(UNIT=9, FILE='ejercicio18.dat', STATUS='old')
    READ(9, 20)
    CLOSE(9)
    20 FORMAT (5(F7.2,1X)) !5 numeros F enteros de 7 lugares con 2 decimales separados por un espacio
   
END PROGRAM 
