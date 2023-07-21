PROGRAM ejercicio29
!COMPILAR ASI: gfortran ejercicio29.f90 -L/usr/lib/x86_64-linux-gnu/ -lnetcdf -lnetcdff -I/usr/include/ -o ejercicio29
!Seguir este ejemplo: https://climate-cms.org/posts/2018-10-12-create-netcdf.html
!Este programa escribe un fichero netCDF con una varible 3D (x, y, tiempo) siguiendo los parametros de una namelist. 
!Tareas:
!(a) La namelist tendrá 4 secciones con los siguientes argumentos (se adjunta ejemplo por claridad):
!(b) Las tres dimensiones tendrán estos atributos: tabla
!(c) La variable se rellenará por filas con números consecutivos (como en 19 y 17), incrementando por múltiples de 10 (it10) a cada paso de tiempo it

    USE netcdf !usa libreria externa netcdf 
    
    IMPLICIT NONE
    
    !variables
    !todas estas vienen de namelist
    INTEGER                     ::dimx, dimy, dimt
    CHARACTER(100)                   ::unidadtiempo, tiemporef
    REAL                        ::inilon, endlon, inilat, endlat, initiempo, endtiempo
    CHARACTER(100)                   ::nombre, nombreestandard, nombrelargo, unidades
    CHARACTER(100)                   ::autor, institucion, pais
    CHARACTER(100)                   ::ficheronc
    !estas no
    CHARACTER(100)                   ::lon_estandard_name, lon_name, lon_units
    CHARACTER(100)                   ::lat_estandard_name, lat_name, lat_units
    CHARACTER(100)                   ::time_estandard_name, time_name, time_units
    !para armar nc
    INTEGER                     ::ide_nc, ide_lon, ide_lat, ide_time, ide_lon_var, ide_lat_var, ide_var, ide_time_var
    INTEGER                     ::minc
    !matriz que se guarda
    REAL, DIMENSION(:,:,:), ALLOCATABLE      ::datos
    INTEGER                                  ::t !para armar los datos
    !vectores con fechas y con lons y lats
    REAL, DIMENSION(:), ALLOCATABLE                          :: time_var
    REAL, DIMENSION(:), ALLOCATABLE                           :: lon_var
    REAL, DIMENSION(:), ALLOCATABLE                           :: lat_var
    INTEGER                                                      :: i !to create the vectores 
    
    NAMELIST /dimensiones/ dimx, dimy, dimt, unidadtiempo, tiemporef, inilon, endlon, inilat, endlat, initiempo, endtiempo                           
    NAMELIST /variable/ nombre, nombreestandard, nombrelargo, unidades
    NAMELIST /atributosglob/ autor, institucion, pais
    NAMELIST /salida/ ficheronc
    
    OPEN(9, FILE="ejercicio29.namelist", STATUS='OLD')
    READ(9, dimensiones)
    READ(9, variable)
    READ(9, atributosglob)
    READ(9, salida)
    CLOSE(9)

    lon_estandard_name = "longitude"
    lon_name = "Longitude"
    lon_units = "degrees_east"
    lat_estandard_name = "latitude"
    lat_name = "Latitude"
    lat_units = "degrees_north"
    time_estandard_name = "time"
    time_name = "Time"
    time_units = unidadtiempo // "since" // tiemporef
    
    !armo matriz de datos
    IF(ALLOCATED(datos)) DEALLOCATE(datos)
    ALLOCATE(datos(dimx, dimy, dimt))
    !La variable se rellenará por filas con números consecutivos (como en 19 y 17), incrementando por múltiples de 10 (it10) a cada paso de tiempo it
    datos = 0. !completo con ceros para probar
    DO t = 1, dimt
       datos(:,:,t) = t * 10
    END DO
    
    !armo vectores con tiempos   
    IF(ALLOCATED(time_var)) DEALLOCATE(time_var)
    ALLOCATE(time_var(dimt))    
    time_var = [(initiempo + REAL(i-1), i=1,dimt)]
    
    !armo vector con longitudes !El paso de las longitudes es: (endlon - inilon)/ dimx 
    IF(ALLOCATED(lon_var)) DEALLOCATE(lon_var)
    ALLOCATE(lon_var(dimx))    
    lon_var = [(inilon + (REAL(i-1)*(endlon - inilon)/ dimx), i=1, dimx)] !VER
    
    !armo vector con latitudes 
    IF(ALLOCATED(lat_var)) DEALLOCATE(lat_var)
    ALLOCATE(lat_var(dimy))    
    lat_var = [(inilat + (REAL(i-1)*(endlat - inilat)/ dimy), i=1,dimy)] !VER
    
    !escribo netcdf
    !genero archivo
    minc = nf90_create(ficheronc, 0, ide_nc) !creo archivo        el 0 es el tipo: A zero value (defined for convenience as NF90_CLOBBER) specifies: overwrite any existing dataset with the same file name,
                                                          !and buffer and cache accesses for efficiency. The dataset will be in netCDF classic format. VER seccion 2.5 de:
                                                          ! https://docs.unidata.ucar.edu/netcdf-fortran/current/f90_datasets.html                                                          
    
    !seteo dimensiones
    minc = nf90_def_dim(ide_nc, lon_name, dimx, ide_lon) ! creo dimension          !IDES las pone automaticamente la libreria
    minc = nf90_def_dim(ide_nc, lat_name, dimy, ide_lat) ! creo dimension
    minc = nf90_def_dim(ide_nc, time_name, dimt, ide_time) ! creo dimension
    
    !seteo variables
    minc = nf90_def_var(ide_nc, lon_name, 0, ide_lon, ide_lon_var)  !0 = NF90_CLOBBER _var
    minc = nf90_def_var(ide_nc, lat_name, 0, ide_lat, ide_lon_var) 
    minc = nf90_def_var(ide_nc, time_name, 0, ide_time, ide_time_var) 
    minc = nf90_def_var(ide_nc, nombre, 0, (/ide_lon, ide_lat, ide_time/), ide_var)
    
    !seteo atributos globales
    minc = nf90_put_att(ide_nc, NF90_GLOBAL, "Autor", autor)
    minc = nf90_put_att(ide_nc, NF90_GLOBAL, "Institucion", institucion)
    minc = nf90_put_att(ide_nc, NF90_GLOBAL, "Pais", pais)
    
    !seteo atributos de las dimensiones
    minc = nf90_put_att(ide_nc, ide_lon_var, "Unit", lon_units) !_var
    minc = nf90_put_att(ide_nc, ide_lat_var, "Unit", lat_units) 
    minc = nf90_put_att(ide_nc, ide_time_var, "Unit", time_units)
    minc = nf90_put_att(ide_nc, ide_lon_var, "StandardName", lon_estandard_name)
    minc = nf90_put_att(ide_nc, ide_lat_var, "StandardName", lat_estandard_name)
    minc = nf90_put_att(ide_nc, ide_time_var, "StandardName", time_estandard_name)
    
    !seteo atributos de la variable
    minc = nf90_put_att(ide_nc, ide_var, "Units", unidades)
    minc = nf90_put_att(ide_nc, ide_var, "StandardName", nombreestandard)
    minc = nf90_put_att(ide_nc, ide_var, "FillValue", -2e8)
    
    !le pongo valores a la variable
    minc = nf90_put_var(ide_nc, ide_lon_var, lon_var)
    minc = nf90_put_var(ide_nc, ide_lat_var, lat_var)
    minc = nf90_put_var(ide_nc, ide_time_var, time_var)
    minc = nf90_put_var(ide_nc, ide_var, datos(:,:,:1:dimt))
    
    !cierro archivo
    minc = nf90_close(ide_nc)

END PROGRAM 



