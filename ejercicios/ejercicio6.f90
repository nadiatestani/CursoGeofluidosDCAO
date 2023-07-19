PROGRAM ejercicio6

! Dada tres coordenadas esfericas r, phi, thita, (phi entre 0 y 360 grados, thita entre -90 y 90 grados) las transforma en coordenadas cartesianas x, y, z

IMPLICIT NONE

REAL                 ::r
REAL                 ::phi
REAL                 ::tita
REAL                 ::x
REAL                 ::y
REAL                 ::z

r = .2
tita = .30
phi = .40

x = r* SIN(tita)*COS(phi) !tendria que pasar tita y phi a radianes
y = r* SIN(tita)*SIN(phi)
z = r* COS(tita)

print *, x, y, z

END PROGRAM
