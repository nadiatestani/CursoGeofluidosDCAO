PROGRAM ejercicio30
!DUDA: NO ME GUARDA las variables en el nc
   !COMPILAR ASI: gfortran ejercicio30.f90 -L/usr/lib/x86_64-linux-gnu/ -lnetcdf -lnetcdff -I/usr/include/ -o ejercicio30

   !Lee datos atmosféricos en formato ASCII siguiendo una estructura tipo GRIB con las siguientes acciones:
   !(a) Leer las variables individualmente
   !(b) Escribir dos funciones que devuelvan el nombre de la variable y del tipo de nivel a partir de las tablas de
       !equivalencias. Usar una instrucción CASE para cada una de ellas
   !(c) calcule los valores mínimo, máximo y valor medio. Imprimir por pantalla los resultados con el formato siguiente:
       ![nombrevariable]: [año]/[mes]/[día] [hora]:[minuto]:[segundo].[milisegundo] en [nombrenivel]: [valornivel]
       ! min: [minimo] max:[maximo] media: [media]
   !(d) Al mismo tiempo re-escribir la misma información, pero en formato NECDF

   !Hay un fichero de datos a leer llamado 'variables_grib.dat', el cuál sigue una estructura GRIB y su cabecera
   !contiene:
   !year, month, day, hour, minute, second, milisecond, varcode, varlevel, level, dimx, dimy, units
   
   USE netcdf !usa libreria externa netcdf 

   IMPLICIT NONE
   
   !variables lectura de datos
   CHARACTER(30)                        ::nombre_archivo
   INTEGER                              ::year, month, day, hour, minute, second, milisecond, varcode, varlevel, level, dimx, dimy !para leer encabezado
   CHARACTER(5)                         ::units !para leer encabezado
   REAL, DIMENSION(:,:), ALLOCATABLE    ::matriz !para guardar la matriz con datos
   INTEGER                              ::i,j
   CHARACTER(30)                        ::nombre_variable
   CHARACTER(30)                        ::tipo_de_nivel
   INTEGER                              ::k
   INTEGER                              ::l 
   REAL, DIMENSION(:,:,:), ALLOCATABLE    ::array !para guardar la array3D con datos
   
   !variables escritura de datos en formato netcdf
   CHARACTER(100)                            ::ficheronc !nombre de archivo nc
   INTEGER                              ::ide_nc, ide_lon, ide_lat, ide_time, ide_lon_var, ide_lat_var, ide_time_var, ide_var
   CHARACTER(100)                            ::time_var
   INTEGER                              ::nc
   INTEGER                              ::n, m
   REAL, DIMENSION(:), ALLOCATABLE      :: array1D_lon
   REAL, DIMENSION(:), ALLOCATABLE      :: array1D_lat
   
   !genero archivo netcdf
   ficheronc = "ejercicio30.nc"
   nc = nf90_create(ficheronc, NF90_NETCDF4, ide_nc) !Para poder mezclar definciones con relleno de archivo se usa formato NF90_NETCDF4
   
   !seteo atributos globales
   nc = nf90_put_att(ide_nc, NF90_GLOBAL, "Autor", "Nadia Testani")
   nc = nf90_put_att(ide_nc, NF90_GLOBAL, "Institucion", "CIMA")
   nc = nf90_put_att(ide_nc, NF90_GLOBAL, "Pais", "Argentina")
     
   recorro_archivo: DO k=1, 10000 
   
      !abro archivos
     OPEN(UNIT=9, FILE='variables_grib.dat', STATUS='old')
      
     !leo encabezado
     READ(9, *,END=100) year, month, day, hour, minute, second, milisecond, varcode, varlevel, level, dimx, dimy, units !--> como no lo cerre sigue leyendo desde donde quedo antes
     
     !dimx y dimy me dan las dimensiones de la matriz de datos, lo seteo
     IF (ALLOCATED(matriz)) DEALLOCATE(matriz)
     ALLOCATE(matriz(dimx, dimy))
   
     !leo los datos correspondientes al encabezado
     cargo_datos: DO i = 1, dimx 
        READ(9, *,END=100) (matriz(i,j),j=1,dimy)     ! j va desde 1 hasta dimy  --> como no lo cerre sigue leyendo desde donde quedo antes
     END DO cargo_datos
   
     !imprimo informacion de variable k 
     PRINT *, "--------------------------------------------------------"
     PRINT *, nombre_variable(varcode), ":", year, "/", month, "/", day, ".", hour, ":", minute, ":", second,".", milisecond, "en"
     PRINT *, tipo_de_nivel(varlevel),":", level
     PRINT *, "min:", MINVAL(matriz), "max:", MAXVAL(matriz), "media:", SUM(matriz)/(dimx*dimy)
     PRINT *, "--------------------------------------------------------"
     
     !guardo en nc
     !creo dimensiones
     nc = nf90_def_dim(ide_nc, "longitud", dimx, ide_lon)         
     nc = nf90_def_dim(ide_nc, "latitud", dimy, ide_lat) 
     nc = nf90_def_dim(ide_nc, "tiempo" , 1, ide_time) 
   
     !seteo variables
     nc = nf90_def_var(ide_nc, "longitud", nf90_real, (/ ide_lon /), ide_lon_var)  !use nf90_real to define the variables
     nc = nf90_def_var(ide_nc, "latitud", nf90_real, (/ ide_lat /), ide_lat_var) 
     nc = nf90_def_var(ide_nc, "tiempo", nf90_real, (/ ide_time /), ide_time_var) 
     nc = nf90_def_var(ide_nc, nombre_variable(varcode), nf90_real, (/ide_lon, ide_lat, ide_time/), ide_var)    
   
     !seteo atributos de las dimensiones
     nc = nf90_put_att(ide_nc, ide_lon_var, "Unit", "degrees_east") 
     nc = nf90_put_att(ide_nc, ide_lat_var, "Unit", "degrees_north") 
     nc = nf90_put_att(ide_nc, ide_time_var, "Unit", "YYYY/MM/DD")
     nc = nf90_put_att(ide_nc, ide_var, "Unit", units)
     
     !valores a las variables
     !le pongo valores a la variable
     time_var = "1996/09/03"
     !armo vectores de lon y lats
     IF (ALLOCATED(array1D_lon)) DEALLOCATE(array1D_lon)
     ALLOCATE(array1D_lon(dimx))
     IF (ALLOCATED(array1D_lat)) DEALLOCATE(array1D_lat)
     ALLOCATE(array1D_lat(dimy))
     DO m = 1, dimx
       array1D_lon(m) = REAL(m)
     END DO
     DO n = 1, dimy
      array1D_lat(n) = REAL(n)
     END DO
     
     nc = nf90_put_var(ide_nc, ide_lon_var, array1D_lon)
     nc = nf90_put_var(ide_nc, ide_lat_var, array1D_lat)
     nc = nf90_put_var(ide_nc, ide_time_var, time_var)
     nc = nf90_put_var(ide_nc, ide_var, matriz(:,:))
      
   END DO recorro_archivo
   100 CONTINUE !interrumpe la lectura cuando las filas del .dat estan vacias xq en el READ puse: END=100
   CLOSE(9)

   !cierro archivo nc
   nc = nf90_close(ide_nc)
    
END PROGRAM ejercicio30

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

