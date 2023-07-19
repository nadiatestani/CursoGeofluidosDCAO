PROGRAM ejercicio9
!Calcular para un vector 2-dimensional su vector ortogonal
!(a,b) * (ao,bo) = 0 --> ao = b, bo = -a

IMPLICIT NONE

REAL, DIMENSION(2,1)                     ::vector
REAL, DIMENSION(2,1)                     ::vector_ortogonal

vector(1,1) = 1
vector(2,1) = 2

vector_ortogonal (1, 1) = vector(2,1) 
vector_ortogonal(2,1) = - vector(1,1) 

PRINT *, vector_ortogonal

END PROGRAM
