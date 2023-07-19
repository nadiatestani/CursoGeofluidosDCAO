PROGRAM basic_write_dat
!Save numbers from 1 to 10 in a .dat, one number per line

IMPLICIT NONE

REAL                          ::numero_inicial
INTEGER                          ::i, numero_final
CHARACTER(30)                    ::filename

filename = "ejemploWriteFacil.dat"

!first number
numero_inicial = 1
numero_final = 10
OPEN(UNIT=9, FILE=filename, STATUS='new')

WRITE(9, 20)(numero_inicial) !WRITE/READ([Nunidad], [formato], END=[numero]) [contenido]
20 FORMAT(10(F5.2,1X)) !no entiendo esto

!the others
cargo_numeros: DO i=INT(numero_inicial+1),numero_final
   OPEN(UNIT=9, FILE=filename, STATUS='old')
   WRITE(9, 20)(REAL(i))
   
END DO cargo_numeros

CLOSE(UNIT=9)
END PROGRAM basic_write_dat


