# Script para leer el útlimo fichero de estación e 
# integrarlo con el fichero correspondiente de tramos

# Granulo: horas
# Dificultad: fácil

library(lubridate)
library(readxl)
library(dplyr)

ultimaEstacion = function(indice = 0){
  #Obtenemos la ruta de la estación más reciente
  ruta_estacion = "/root/proyecto2020/demonioEstacion/estacion/"
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
  nombre = paste0("/root/proyecto2020/demonioIntegracion/cadaHora/","integracion",nombre)
  return(nombre)
}
#obtenerNombreFicheroIntegrado(1)

obtenerFechaEstacion = function(indice = 0){
  #Obtenemos el la fecha de la estacion
  fecha = strsplit(ultimaEstacion(indice),"/")
  fecha = fecha[[1]][length(fecha[[1]])]
  fecha = substr(fecha,10,nchar(fecha)-4)
  return(fecha)
}
#fecha = obtenerFechaEstacion(1)

obtenerFechaFormateada = function(fecha){
  #Obtener la fecha en objeto datetime
  parte1 = substr(fecha,1,10)
  parte2 = substr(fecha,12,nchar(fecha))
  parte2 = gsub("-",":",parte2)
  fecha = mdy_hms(paste0(parte1," ",parte2))
  return(fecha)
}
#obtenerFechaFormateada(fecha)

listarTramos = function(){
  #Listamos los tramos de la carpeta tramos
  ruta_estacion = "/root/proyecto2020/demonioEspiras/tramosPorHora/"
  files = list.files(ruta_estacion)
  files = sort(files,decreasing = T)
  return(files)
}
#listarTramos()

obtenerFechaTramo = function(tramo){
  #Obtenemos el la fecha del tramo
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
  ruta = "/root/proyecto2020/demonioIntegracion/cadaHora/"
  ruta_historico = "/root/proyecto2020/demonioIntegracion/cadaHoraHistorico/"
  ficheros = list.files(ruta)
  ficheros = sort(ficheros)
  cantidad_ficheros = length(ficheros)
  ficheros_mover_bool= FALSE
  max = 24
  if (cantidad_ficheros > max){
	ficheros_mover_bool = TRUE
    cantidad_mover = cantidad_ficheros - max
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
  fecha_estacion_anterior = obtenerFechaFormateada(obtenerFechaEstacion(1))
  intervalo_fechas = interval(fecha_estacion_anterior, fecha_estacion)
  ruta_tramo = "/root/proyecto2020/demonioEspiras/tramosPorHora/"
  primer_archivo = TRUE
  encontrados = FALSE
  for (file in listarTramos()){
    fecha = obtenerFechaFormateada(obtenerFechaTramo(file))
    if (fecha %within% intervalo_fechas){
      encontrados = TRUE
      ruta_read_tramo = paste0(ruta_tramo,file)
      if (primer_archivo){
        contador_ficheros =  1
        primer_archivo = FALSE
        fichero_final = read.csv2(ruta_read_tramo,fileEncoding = "UTF-8",stringsAsFactors = FALSE)
        fichero_final = fichero_final[order(fichero_final$des_tramo, fichero_final$latitud),]
        fichero_final$lectura[is.na(fichero_final$lectura)] <- 0 
      }else{
        contador_ficheros = contador_ficheros + 1
        dataframe = read.csv2(ruta_read_tramo,fileEncoding = "UTF-8",stringsAsFactors = FALSE)
        fichero_final = unificarFicheros(fichero_final,dataframe)
      }
      
    }else if (encontrados){ #fast path 
      break
    }
  }
  fichero_final$lectura = fichero_final$lectura %/% contador_ficheros
  fichero_final$nombre = tabla_correspondencias$nombre
  estaciones = obtenerDataframeEstacion(ultimaEstacion())
  for (i in 1:nrow(fichero_final)){
    nombre = fichero_final[i,5]
    fichero_final[i,5:17] = estaciones[nombre,]
  }
  #fichero_final = fichero_final %>% mutate_if(is.double, round, digits=5)
  fileName = paste0("/root/proyecto2020/demonioIntegracion/cadaHora/hora_",obtenerFechaEstacion(),".csv")
  write.csv2(fichero_final,fileName, row.names = F,quote=F)
  moverFicheros()
}
obtenerTramoCorrespondiente()


#Para el hsitórico que teniamos de antes, se ha integrado de la siguiente forma:
ruta_estacion = "/Users/lisagilyarovskaya/Downloads/historico_final/horas/estaciones/"
files = list.files(ruta_estacion)
files= sort(files, decreasing = T)
for (i in 1:length(files)){
  obtenerTramoCorrespondiente()
  ruta = paste0("/Users/lisagilyarovskaya/Downloads/historico_final/horas/estaciones/",files[[i]])
  file.remove(ruta)
  #probar remain
}
