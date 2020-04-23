# Script para leer el útlimo fichero de estación e 
# integrarlo con el fichero correspondiente de tramos

# Granulo: horas
# Dificultad: fácil

library(lubridate)
library(readxl)
library(dplyr)

ultimaEstacion = function(indice = 0){
  #Obtenemos la ruta de la estación más reciente
  ruta_estacion = "/root/proyecto2020/demonioEstacion/estacionesPorDia/"
  files = list.files(ruta_estacion)
  files = sort(files)
  filepath = paste0(ruta_estacion,files[length(files)-indice])
  return(filepath)
}
#ultimaEstacion(1)

obtenerNombreFicheroIntegrado = function(indice = 0){
  #Obtenemos el nombre del fichero que integrará tramos y estaciones
  nombre = strsplit(ultimaEstacion(indice),"/")
  nombre = nombre[[1]][length(nombre[[1]])]
  nombre = substr(nombre,9,nchar(nombre)-4)
  nombre = paste0("/root/proyecto2020/demonioIntegracion/cadaDia/","dia",nombre)
  return(nombre)
}
#obtenerNombreFicheroIntegrado(0)

obtenerFechaEstacion = function(indice = 0){
  #Obtnemos la fecha de la estacion
  fecha = strsplit(ultimaEstacion(indice),"/")
  fecha = fecha[[1]][length(fecha[[1]])]
  fecha = substr(fecha,10,nchar(fecha)-4)
  return(fecha)
}
#fecha =obtenerFechaEstacion(1)

obtenerFechaFormateada = function(fecha){
  #Obtener la fecha en objeto datetime

  #fecha = mdy_hms(paste0(parte1," ",parte)
  fecha = ymd(fecha)
  return(fecha)
}
#obtenerFechaFormateada(fecha)

listarTramos = function(){
  #Listamos los tramos de la carpeta tramos
  ruta_estacion = "/root/proyecto2020/demonioEspiras/tramosPorDia/"
  files = list.files(ruta_estacion)
  files = sort(files,decreasing = T)
  return(files)
}
#listarTramos()

obtenerFechaTramo = function(tramo){
  #Obtenemos la fecha del tramo
  fecha = substr(tramo,8,nchar(tramo)-4)
  return(fecha)
}
#obtenerFechaTramo(listarTramos()[1])

obtenerTablaCorrepondencias = function(){
  dataframe = read_xlsx("/root/proyecto2020/demonioIntegracion/tramosEstacionesCorrespondiente.xlsx")
  return(dataframe)
}
#obtenerTablaCorrepondencias()

obtenerDataframeEstacion = function(path){
  dataframe = read.csv(
    file = path,
    stringsAsFactors = FALSE, 
    sep = ';',
    encoding = "UTF-8")
  row.names(dataframe) = dataframe$nombre
  return(dataframe)
}
#dataframe = obtenerDataframeEstacion(ultimaEstacion())


unificarFicheros = function(fichero_final,dataframe){
  dataframe = dataframe[order(dataframe$des_tramo, dataframe$latitud),]
  dataframe$lectura[is.na(dataframe$lectura)] <- 0 
  fichero_final$lectura = fichero_final$lectura + dataframe$lectura
  return(fichero_final)
  
}

moverFicheros = function(){
  ruta = "/root/proyecto2020/demonioIntegracion/cadaDia/"
  ruta_historico = "/root/proyecto2020/demonioIntegracion/cadaDiaHistorico/"
  ficheros = list.files(ruta)
  ficheros = sort(ficheros)
  cantidad_ficheros = length(ficheros)
  ficheros_mover_bool = FALSE
  if (cantidad_ficheros > 15){
    ficheros_mover_bool = TRUE
    cantidad_mover = cantidad_ficheros - 15
    ficheros_mover = ficheros[1:cantidad_mover]
  }
  if (ficheros_mover_bool){
  for (fichero in ficheros_mover){
    pathFichero = paste0(ruta,fichero)
    pathHistorico = paste0(ruta_historico,fichero)
    file.rename(pathFichero, pathHistorico)}
  }
  }
#moverFicheros()


obtenerTramoCorrespondiente = function(){
  #Obtener tramo correspodiente a la estación actual
  tabla_correspondencias = obtenerTablaCorrepondencias()
  fecha_estacion = obtenerFechaFormateada(obtenerFechaEstacion())
  ruta_tramo = "/root/proyecto2020/demonioEspiras/tramosPorDia/"
  primer_archivo = TRUE
  encontrados = FALSE
  for (file in listarTramos()){
    fecha = obtenerFechaFormateada(obtenerFechaTramo(file))
    if (fecha == fecha_estacion){
      encontrados = TRUE
      ruta_read_tramo = paste0(ruta_tramo,file)
      if (primer_archivo){
        primer_archivo = FALSE
        fichero_final = read.csv2(ruta_read_tramo,fileEncoding = "UTF-8",stringsAsFactors = F)
        fichero_final = fichero_final[order(fichero_final$des_tramo, fichero_final$latitud),]
        fichero_final$lectura[is.na(fichero_final$lectura)] <- 0 
      }else{
        dataframe = read.csv2(ruta_read_tramo,fileEncoding = "UTF-8",stringsAsFactors = F)
        fichero_final = unificarFicheros(fichero_final,dataframe)
      }
      
    }else if (encontrados){ #fast path 
      break
    }
  }
  fichero_final$nombre = tabla_correspondencias$nombre
  estaciones = obtenerDataframeEstacion(ultimaEstacion())
  for (i in 1:nrow(fichero_final)){
    nombre = fichero_final[i,5]
    fichero_final[i,5:17] = estaciones[nombre,]
  }
  fileName = paste0("/root/proyecto2020/demonioIntegracion/cadaDia/dia_",obtenerFechaEstacion(),".csv")
  write.csv2(fichero_final,fileName, row.names = F,quote=F)
  moverFicheros()
  
  
}
obtenerTramoCorrespondiente()




#Para el hsitórico que teniamos de antes, se ha integrado de la siguiente forma:
ruta_estacion = "/Users/lisagilyarovskaya/Downloads/historico_final/dias/estaciones/"
files = list.files(ruta_estacion)
files= sort(files, decreasing = T)
for (i in 1:length(files)){
  obtenerTramoCorrespondiente()
  ruta = paste0("/Users/lisagilyarovskaya/Downloads/historico_final/dias/estaciones/",files[[i]])
  file.remove(ruta)
}

