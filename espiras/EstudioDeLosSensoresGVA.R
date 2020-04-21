library(readr)
library(stringr)
library(ggplot2)
library(gridExtra)

por_dia = read_tsv('./MDEST462500462020.txt')
por_dia = as.data.frame(por_dia)
por_dia

a = setwd('./PROY 2/estacionesCSV/')
b = list.files(path = a)

options(digits = 2)

mifuncion <- function(nombre, b) {
  
  almacen = data.frame()
  tabla_final = data.frame()
  
  
  iter = 1
  cont = 1
  fila = 1
  diam = c()
  
  for (fichero in b) {
    fichero <- as.data.frame(read.csv(file = fichero, sep = ';'))
    
    dia = str_sub(b[cont], 13, 14)
    mes = str_sub(b[cont], 10, 11)
    so2 = fichero[fichero$nombre == nombre,5]
    no2 = fichero[fichero$nombre == nombre,6]
    o3 = fichero[fichero$nombre == nombre,7]
    co = fichero[fichero$nombre == nombre,8]
    pm10 = fichero[fichero$nombre == nombre,9]
    pm25 = fichero[fichero$nombre == nombre,10]
    almacen[iter,1:7] = list(paste(dia,'_', mes), so2, no2, o3, co, pm10, pm25)
    print(almacen)
    if (cont + 1 > length(b) | as.numeric(str_sub(b[cont+1], 13, 14)) != as.numeric(dia)){
      for(i in 2:ncol(almacen)){
        almacen[is.na(almacen[,i]), i] <- mean(almacen[,i], na.rm = TRUE)
      }
      almacen = colMeans(almacen[,-1])
      diam = c(diam, paste(dia,'_',mes))
      tabla_final = rbind(tabla_final, almacen)
      almacen = data.frame()
      fila = fila + 1
      iter = 1
    }
    else {
      iter = iter + 1
    }
    cont = cont + 1
    
  }
  tabla_final = cbind(tabla_final, diam)
  names(tabla_final) = c('so2', 'no2', 'o3', 'co', 'pm10', 'pm25', 'dia_mes')
  return(tabla_final)
}

x = mifuncion('Pista de Silla', b)
x

estaciones = x[1:13,1:7]
por_dia = por_dia[79:91,1:8]

days = c(por_dia[,1])

por_dia = por_dia[,-c(1,3,6)]
estaciones = estaciones[,-c(4,7)]

o3 = por_dia[,4]
por_dia = por_dia[,-c(4)]
por_dia = cbind(por_dia, o3)

o3 = estaciones[,3]
estaciones = estaciones[,-c(3)]
estaciones = cbind(estaciones, o3)

por_dia = cbind(por_dia, days)
estaciones = cbind(estaciones, days)

names(por_dia) = colnames(estaciones)
valores = colnames(estaciones[,-6])

for (i in valores){
  
  id = 1:nrow(por_dia)
  grafico = ggplot(data=por_dia, aes_string('days', i, group = 1)) +
    geom_line(colour="red", size=0.3) +
    geom_point(colour="red", size=0.5, shape=21, fill="white") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  generalitat = grafico + ggtitle('GENERALITAT', i)
  
  id = 1:nrow(estaciones)
  grafico = ggplot(data=estaciones, aes_string('days', i, group = 1)) +
    geom_line(colour="red", size=0.3) +
    geom_point(colour="red", size=0.5, shape=21, fill="white") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  grafico = grafico + ggtitle('ESTACIONES', i)
  
  grid.arrange(generalitat, grafico, ncol=2)

}
