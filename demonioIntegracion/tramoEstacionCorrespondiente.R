library(dplyr)
library(ggplot2)
library(flux)
library (readxl)
library(xlsx)



estaciones = readxl::read_xlsx("C:/root/proyecto2020/demonioEstacion/nombreEstacionCoordenadas.xlsx")
tramos = readxl::read_xlsx("C:/root/proyecto2020/demonioEstacion/tramosNombreCoordenadas.xlsx")

tramos = tramos[!duplicated(tramos[,c('des_tramo')]),]

for (i in 1:nrow(tramos)){
  options(digits = 10)
  latitud = tramos[i,2]
  longitud = tramos[i,3]
  current_min = 100
  current_name = "Ninguna"
  for (j in 1:nrow(estaciones)){
    lat_est = estaciones[j,2]
    lon_est = estaciones[j,3]
    cateto1 = abs(latitud - lat_est)
    cateto2 = abs(longitud - lon_est)
    suma_catetos = cateto1^2 + cateto2^2
    dist = sqrt(suma_catetos)
    if (dist < current_min){
      current_min = dist
      current_name = estaciones[j,1]

    }}
  tramos[i,4] = current_name
    
}
estaciones[2,1]


write.xlsx(tramos,"C:\root\proyecto2020\demonioEstaciontramosEstacionesCorrespondiente.xlsx")
