# emis_puntual
!   
!	Created by Agustin Garcia on 28/08/2012.
!

    Programa Adiciona e interpola datos

Este programa adiciona emisiones del archivo
new_emiss.csv  al archivo de emisiones wrfchemin.nc creando un nuevo archivo 
que es el wrfchemi_00z_d01 o wrfchemi_12z_d01 dependiendo de la hora inicial 
del wrfchemin

Archivos de entrada requeridos:
   new_emiss.csv
   wrfchemin.nc

Archivo generado:
    wrfchemi_##z_d01
    
Para compilar

make

Editar el Makefile para indicar compilador y librerias

variable de ambiente requerida NETCDF=/directorio_donde_esta_NETCDF

formato del new_emiss.csv
2,19,Latitud Longitud,Hr_ini,Hr_fin,E_SO2,E_CO,E_NO,E_HC3,E_PM25,E_PM_10,E_CH4,E_CSL,E_ETH,E_CO2,E_ORA1,E_HCHO,E_ALD,E_ORA2,E_TOL,E_XYL,E_OL2,E_HC5,E_HC8
19.21888889	-98.11305556	2	0	1	581	100800	4550	4760	8400	9800	4760	511	4060	1064700	1260	1050	0.105	140	112	40.6	91	105	980
19.14277778	-98.15361111	2	0	1	581	100800	4550	4760	8400	9800	4760	511	4060	1064700	1260	1050	0.105	140	112	40.6	91	105	980

El primer numero en el encabezado indica el numero de reglones a leer, el segundo el numero de variables del mecanismo a leer. Los nombres de las variables son los del archivo de emisiones y estan en kg/hora
a partir del segundo renglon se inicia con
localizacion  (lat, lon)
capa de liberacion (el valor maximo depende del emissions_zdim_stag del wrfchemin)
hora inicial y final en UTM
emision del compuesto en kg/hr

common error:

global attribute mecha is not a common one

ncatted -O -a MECHANISM,global,a,c,”RADM2\n” wrfchemin.nc
