PROGRAM ejercicio8
! Realizar un progrma que pase todas las letras mayúsculas a minúculas y vicerversa de una palabra
!ICHAR(’A’) = 65 ... ICHAR(’Z’) = 90; ICHAR(’a’) = 97 ... ICHAR(’z’) = 122

! aca uso IF para chequear si es mayuscula o minuscula

IMPLICIT NONE

CHARACTER(len=30)                 ::palabra
CHARACTER(len=30)                 ::nueva_palabra
INTEGER                           ::pos_letra

palabra = "PerrO"
intercambio: DO pos_letra = 1, LEN(palabra)

chequeo_min_may: IF (ICHAR(palabra(pos_letra:pos_letra)) >= 65 .and. ICHAR(palabra(pos_letra:pos_letra)) <= 90  ) THEN
  nueva_palabra(pos_letra:pos_letra) = CHAR(ICHAR(palabra(pos_letra:pos_letra)) + 32)
ELSE
   nueva_palabra(pos_letra:pos_letra) = CHAR(ICHAR(palabra(pos_letra:pos_letra)) - 32)
END IF chequeo_min_may
  
END DO intercambio

print *, nueva_palabra

END PROGRAM

