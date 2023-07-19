PROGRAM ejercicio18
!simula un tiro parabolico con la posición inicial (m), la velocidad
!inicial (ms −1 ) y el paso de timpo (s).

!devuelve un fichero que contiene 5 columnas y para cada paso de timepo contiene: (1) tiempo, (2) posicion x, (3) posicion y, (4) velocidad x, (5) velocidad y.

!Ecuaciones:
! v (t) = V (t − 1) + G δt; 
! G = (0., −9.81)
! P(t) = P(t − 1) + V(t)δt

   IMPLICIT NONE

   !variables
   REAL                 ::velocidadx !funcion
   REAL                 ::velocidady !funcion
   REAL                 ::posicionx !funcion
   REAL                 ::posiciony !funcion
   REAL, DIMENSION(2)                ::posicion_inicial !(Px,Py) en metros
   REAL, DIMENSION(2)                ::velocidad_inicial !(Vx, Vy) en metros/segundos
   REAL                              ::paso_del_tiempo !paso del tiempo en segundos
   REAL                              ::gravedad !gravedad, escalar en y
   REAL                              ::velocidadx_anterior !en funcion velocidadx
   REAL                              ::velocidady_anterior !en funcion velocidady
   REAL                              ::posicionx_anterior !en funcion posicionx
   REAL                              ::posiciony_anterior !en funcion posiciony
   REAL                              ::tiempo !

   !opero
   
   !valores iniciales
   posicion_inicial(1:2) = (/0., 100./)
   velocidad_inicial(1:2) = (/5., 5./)
   paso_del_tiempo = 1.
   gravedad = -9.81
   
   !para iniciar el loop
   posicionx_anterior = posicion_inicial(1)
   posiciony_anterior = posicion_inicial(2)
   velocidadx_anterior = velocidad_inicial(1)
   velocidady_anterior = velocidad_inicial(2)
   tiempo = 0
   
   !ahora el loop 
   OPEN(UNIT=9, FILE='ejercicio18.dat', STATUS='new')
   DO WHILE (posiciony_anterior >= 0) !va a hacer los calculos hasta que el objeto toque el piso
       WRITE(9,20)(/tiempo, posicionx_anterior, posiciony_anterior, velocidadx_anterior, velocidady_anterior/)
       !genera nuevos valores
       velocidadx_anterior = velocidadx(velocidadx_anterior, paso_del_tiempo, gravedad)
       velocidady_anterior = velocidady(velocidady_anterior, paso_del_tiempo, gravedad)
       posicionx_anterior = posicionx(posicionx_anterior, paso_del_tiempo, velocidadx_anterior)
       posiciony_anterior = posiciony(posiciony_anterior, paso_del_tiempo, velocidady_anterior)
       tiempo = tiempo + paso_del_tiempo
   END DO
   CLOSE(UNIT=9)
   20 FORMAT (5(F7.2,1X)) !5 numeros F enteros de 7 lugares con 2 decimales separados por un espacio
   
   
END PROGRAM ejercicio18





!funcion que calcula la velocidad x
REAL FUNCTION velocidadx(velocidadx_anterior, paso_del_tiempo, gravedad)

     IMPLICIT NONE
     REAL, INTENT(in)     ::velocidadx_anterior
     REAL, INTENT(in)     ::paso_del_tiempo
     REAL, INTENT(in)     ::gravedad

     velocidadx = velocidadx_anterior !no cambia, lo programo igual para que quede el boceto general 
     RETURN

END FUNCTION

!funcion que calcula la velocidad y
REAL FUNCTION velocidady(velocidady_anterior, paso_del_tiempo, gravedad)

     IMPLICIT NONE
     REAL, INTENT(in)     ::velocidady_anterior
     REAL, INTENT(in)     ::paso_del_tiempo
     REAL, INTENT(in)     ::gravedad

     velocidady = velocidady_anterior + gravedad * paso_del_tiempo
     RETURN

END FUNCTION

!funcion que calcula posicion x
REAL FUNCTION posicionx(posicionx_anterior, paso_del_tiempo, velocidadx)

     IMPLICIT NONE
     REAL, INTENT(in)     ::posicionx_anterior
     REAL, INTENT(in)     ::paso_del_tiempo
     REAL, INTENT(in)     ::velocidadx

     posicionx = posicionx_anterior + velocidadx * paso_del_tiempo
     RETURN

END FUNCTION

!funcion que calcula posicion y
REAL FUNCTION posiciony(posiciony_anterior, paso_del_tiempo, velocidady)

     IMPLICIT NONE
     REAL, INTENT(in)     ::posiciony_anterior
     REAL, INTENT(in)     ::paso_del_tiempo
     REAL, INTENT(in)     ::velocidady

     posiciony = posiciony_anterior + velocidady * paso_del_tiempo 
     RETURN

END FUNCTION


