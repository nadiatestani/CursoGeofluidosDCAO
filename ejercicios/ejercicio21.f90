PROGRAM ejercicio21
!DUDA: que es varlevel y level, con cual deberia decodificar el tipo de nivel?
!DUDA: como lo escribo en un binario de fortran??? COMO LO HICE ME DA ESTE ERROR: Fortran runtime error: Format present for UNFORMATTED data transfer


!Lee datos atmosféricos en formato ASCII siguiendo una estructura tipo GRIB con las siguientes acciones:
!(a) Leer las variables individualmente
!(b) Escribir dos funciones que devuelvan el nombre de la variable y del tipo de nivel a partir de las tablas de
    !equivalencias. Usar una instrucción CASE para cada una de ellas
!(c) calcule los valores mínimo, máximo y valor medio. Imprimir por pantalla los resultados con el formato siguiente:
    ![nombrevariable]: [año]/[mes]/[día] [hora]:[minuto]:[segundo].[milisegundo] en [nombrenivel]: [valornivel]
    ! min: [minimo] max:[maximo] media: [media]
!(d) Al mismo tiempo re-escribir la misma información, pero en formato binario de Fortran.

!Hay un fichero de datos a leer llamado 'variables_grib.dat', el cuál sigue una estructura GRIB y su cabecera
!contiene:
!year, month, day, hour, minute, second, milisecond, varcode, varlevel, level, dimx, dimy, units


   IMPLICIT NONE
   
   CHARACTER(30)                        ::nombre_archivo
   INTEGER                              ::year, month, day, hour, minute, second, milisecond, varcode, varlevel, level, dimx, dimy !para leer encabezado
   CHARACTER(5)                         ::units !para leer encabezado
   REAL, DIMENSION(:,:), ALLOCATABLE    ::matriz !para guardar la matriz con datos
   INTEGER                              ::i,j
   CHARACTER(30)                        ::nombre_variable
   CHARACTER(30)                        ::tipo_de_nivel
   INTEGER                              ::k
   
   DO k=1, 10000
   
   !abro archivos
   OPEN(UNIT=9, FILE='variables_grib.dat', STATUS='old')
   !OPEN(UNIT=10, FILE = "variables_grib.bin", STATUS='new', FORM='unformatted') !para escribir el binario con los datos
   
   !leo primer encabezado
   READ(9, *,END=100) year, month, day, hour, minute, second, milisecond, varcode, varlevel, level, dimx, dimy, units !--> como no lo cerre sigue leyendo desde donde quedo antes
   !WRITE(10,*) year, month, day, hour, minute, second, milisecond, varcode, varlevel, level, dimx, dimy, units !lo escribo en el binario
   
   !dimx y dimy me dan las dimensiones de la matriz de datos, lo seteo
   IF (ALLOCATED(matriz)) DEALLOCATE(matriz)
   ALLOCATE(matriz(dimx, dimy))
   
   !leo los datos correspondientes a el primer encabezado
   OPEN(UNIT=9, FILE='variables_grib.dat', STATUS='old')
   DO i = 1, dimx !le sumo 1 porque va a leer la fila de encabezado tmb
      READ(9, *,END=100) (matriz(i,j),j=1,dimy)     ! j va desde 1 hasta dimy  --> como no lo cerre sigue leyendo desde donde quedo antes
      !WRITE(10,*) (matriz(i,j),j=1,dimy) !lo escribo en el binario
   END DO
   
   !imprimo todo lo que tiene que ver con la primera variable
   PRINT *, "--------------------------------------------------------"
   PRINT *, nombre_variable(varcode), ":", year, "/", month, "/", day, ".", hour, ":", minute, ":", second,".", milisecond, "en"
   PRINT *, tipo_de_nivel(varlevel),":", level
   PRINT *, "min:", MINVAL(matriz), "max:", MAXVAL(matriz), "media:", SUM(matriz)/(dimx*dimy)
   PRINT *, "--------------------------------------------------------"
   
   END DO
   100 CONTINUE
   CLOSE(9)
   !CLOSE(10)
  
  !loop con esto que se interrumpe cuando year = "" 
  !o esto que paso lluis para cuando no sabemos la cantidad de filas 
END PROGRAM

CHARACTER(30) FUNCTION nombre_variable(varcode)

    INTEGER, INTENT(in)     ::varcode

    SELECT CASE(varcode)
        CASE(34)
            nombre_variable = "sst"
        CASE(39)
            nombre_variable = "wsoil1"
        CASE(40)
            nombre_variable = "wsoil2"
        CASE(41)
            nombre_variable = "wsoil3"
        CASE(42)
            nombre_variable = "wsoil4"
        CASE(129)
            nombre_variable = "zg"
        CASE(130)
            nombre_variable = "ta"
        CASE(131)
            nombre_variable = "ua"
        CASE(132)
            nombre_variable = "va"
        CASE(134)
            nombre_variable = "ps"
        CASE(139)
            nombre_variable = "tsoil1"
        CASE(151)
            nombre_variable = "mslp"
        CASE(157)
            nombre_variable = "hur"
        CASE(165)
            nombre_variable = "uas"
        CASE(166)
            nombre_variable = "vas"
        CASE(168)
            nombre_variable = "tas"
        CASE(170)
            nombre_variable = "tsoil2"
        CASE(183)
            nombre_variable = "tsoil3"
        CASE(228)
            nombre_variable = "pr"
        CASE(235)
            nombre_variable = "tsk"
        CASE(236)
            nombre_variable = "tsoil4"
        CASE DEFAULT
            nombre_variable = "El nombre de la variable no esta codificado"
    END SELECT
    
RETURN

END FUNCTION


CHARACTER(30) FUNCTION tipo_de_nivel(varlevel)

    INTEGER, INTENT(in)     ::varlevel

    SELECT CASE(varlevel)
        CASE(1)
            tipo_de_nivel = "surface"
        CASE(3)
            tipo_de_nivel = "pressure"
        CASE(4)
            tipo_de_nivel = "depth"
        CASE(5)
            tipo_de_nivel = "eta"
        CASE DEFAULT
            tipo_de_nivel = "El tipo de nivel no esta codificado"
    END SELECT
    
RETURN

END FUNCTION


