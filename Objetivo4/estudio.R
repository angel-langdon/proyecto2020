library(leaflet)
library(leaflet.extras)
require(ggplot2)
library(readxl)
library(corrplot)
library(caret)
library(ropls)
library(pls)
library(gridExtra)

filePath <- function(number,fase){
  if (fase == "fasex"){
    granulo="fasex/"
  }else if(fase == "fase0"){
    granulo = "fase0/"}
  else if (fase == "fase1"){
    granulo = "fase1/"
  }
  
  
  path = paste0("./",granulo)
  files = list.files(path)
  files = sort(files)
  filepath = paste0(path,files[number])
  return(filepath)
}

#filePath(1,"fasex")

fileTable <- function(path){
  dataframe = read.csv(
    file = path,
    stringsAsFactors = FALSE, 
    sep = ';',
    encoding = "UTF-8")
  dataframe = dataframe[complete.cases(dataframe), ]
  if ("lectura" %in% colnames(dataframe)){
    names(dataframe)[names(dataframe) == 'lectura'] <- 'ih'
  }
  return(dataframe)
}
#z =fileTable(filePath(1,"fasex"))

listarFicheros = function(fase){
  if (fase == "fasex"){
    granulo="fasex/"
  }else if(fase == "fase0"){
    granulo = "fase0/"}
  else if (fase == "fase1"){
    granulo = "fase1/"
  }

  path = paste0("./",granulo)
  files = list.files(path)
  return(sort(files))
  
}
#listarFicheros("fase1")
#fileTable(filePath(1,"fasex"))
#fichero = 1
mediaTramosDia = function(){
  medias = data.frame()
  i = 1
  for (fichero in 1:length(listarFicheros("fasex"))){
    medias[fichero,"dia"] = i
    i = i+1
    df = fileTable(filePath(fichero,"fasex"))
    media = mean(as.numeric(df$ih),na.rm = T)
    medias[fichero,"Confinamiento"] = round(media) 
  }
  for (fichero in 1:length(listarFicheros("fase0"))){
    df = fileTable(filePath(fichero,"fase0"))
    media = mean(as.numeric(df$ih),na.rm = T)
    medias[fichero,"Fase_0"] = round(media) 
  }
  for (fichero in 1:length(listarFicheros("fase1"))){
    df = fileTable(filePath(fichero,"fase1"))
    media = mean(as.numeric(df$ih),na.rm = T)
    medias[fichero,"Fase_1"] = round(media) 
  }
  
  return(medias)
  
}
suma = mediaTramosDia()

traficoPS = function(){
  #Tr?fico medio de pista de silla
  medias = data.frame()
  i = 1
  for (fichero in 1:length(listarFicheros("dias",2017))){
    medias[fichero,"dia"] = i
    i = i+1
    df = fileTable(filePath(fichero,"dias",2017))
    df = df[ df$idpm == '518' | df$idpm == '504' ,]
    media = mean(as.numeric(df$ih),na.rm = T)
    medias[fichero,"2017"] = round(media) 
  }
  for (fichero in 1:length(listarFicheros("dias",2020))){
    df = fileTable(filePath(fichero,"dias",2020))
    df = df[ df$idpm == '518' | df$idpm == '504' ,]
    media = mean(as.numeric(df$ih),na.rm = T)
    medias[fichero,"2020"] = round(media) 
  }
  return(medias)
}
#traficoPS()


###########################################
timeSeries1 = function(df){
  cols <- c("Confinamiento"="red","Fase 0"="blue","Fase 1"="green")
  a = ggplot(df) +                    # basic graphical object
    geom_line(aes(x=df$dia, y=df$"Confinamiento", colour="Confinamiento"),cex = 1.2) +
    geom_line(aes(x=df$dia, y=df$"Fase_0", colour="Fase 0"),cex = 1.2) +
    geom_line(aes(x=df$dia, y=df$"Fase_1", colour="Fase 1"),cex = 1.2) +
    xlab("Día") + ylab("Media coches/día por tramo") + ggtitle("Promedio coches/día por tramo")+
    scale_colour_manual("",values=cols)+
    scale_x_continuous(breaks=c(1, 2, 3,4,5,6,7),
                     labels=c("Lunes","Martes","Miercoles","Jueves","Viernes","Sábado",
                              "Domingo"))+
    theme(axis.text=element_text(size=10),
          axis.title=element_text(size=12),
          plot.title = element_text(hjust = 0.5,size = 14)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
    
  return(a)
}
#timeSeries1(suma)
filePathCont <- function(number,fase){
  if (fase == "fasex"){
    granulo="cont/fasex/"
  }else if(fase == "fase0"){
    granulo = "cont/fase0/"}
  else if (fase == "fase1"){
    granulo = "cont/fase1/"
  }
  
  
  path = paste0("./",granulo)
  files = list.files(path)
  files = sort(files)
  filepath = paste0(path,files[number])
  return(filepath)
}
# filePathCont(1,"fasex")
mediaContDia = function(contaminante){
  medias = data.frame()
  i = 1
  for (fichero in 1:length(listarFicheros("fasex"))){
    medias[fichero,"dia"] = i
    i = i+1
    df = fileTable(filePathCont(fichero,"fasex"))
    media = mean(as.numeric(sub(",",".",df[,contaminante])),na.rm = T)
    medias[fichero,"Confinamiento"] = round(media,2) 
  }
  for (fichero in 1:length(listarFicheros("fase0"))){
    df = fileTable(filePathCont(fichero,"fase0"))
    media = mean(as.numeric(sub(",",".",df[,contaminante])),na.rm = T)
    medias[fichero,"Fase_0"] = round(media,2) 
  }
  for (fichero in 1:length(listarFicheros("fase1"))){
    df = fileTable(filePathCont(fichero,"fase1"))
    media = mean(as.numeric(sub(",",".",df[,contaminante])),na.rm = T)
    medias[fichero,"Fase_1"] = round(media,2) 
  }
  
  return(medias)
  
}
suma = mediaContDia("no2")



#df = contaminanteDia("pm10")
# pm10 = microgramos/metro_cubico
# no2 =  microgramos/metro_cubico
# so2 =  microgramos/metro_cubico
# pm25 =  microgramos/metro_cubico
# o3 =  microgramos/metro_cubico



timeSeriesContaminantes = function(df,contaminante){
  y_lab = paste0(contaminante, " microgramos/m3")
  gg_title = paste0("Niveles: ",contaminante)
  cols <- c("Confinamiento"="red","Fase 0"="blue","Fase 1"="green")
  y2017 = as.numeric(df[,2])
  y2020 = df[,3]
  a = ggplot(df) +                    # basic graphical object
    geom_line(aes(x=df$dia, y=df$"Confinamiento", colour="Confinamiento"),cex = 1.2) +
    geom_line(aes(x=df$dia, y=df$"Fase_0", colour="Fase 0"),cex = 1.2) +
    geom_line(aes(x=df$dia, y=df$"Fase_1", colour="Fase 1"),cex = 1.2) +
    xlab("Día") + ylab(y_lab) + ggtitle(gg_title)+
    scale_colour_manual("",values=cols)+
    scale_x_continuous(breaks=c(1, 2, 3,4,5,6,7),
                       labels=c("Lunes","Martes","Miercoles","Jueves","Viernes","Sábado",
                                "Domingo"))+
    theme(axis.text=element_text(size=10),
          axis.title=element_text(size=12),
          plot.title = element_text(hjust = 0.5,size = 14)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
  

  return(a)
  
}
# timeSeriesContaminantes(mediaContDia("pm10"),"pm10")
 # timeSeriesContaminantes(mediaContDia("pm25"),"pm2.5")
 # timeSeriesContaminantes(mediaContDia("no2"),"no2")
 # timeSeriesContaminantes(mediaContDia("so2"),"so2")
 # timeSeriesContaminantes(mediaContDia("o3"),"o3")


filePathCont1 <- function(number,fase){
  if (fase == "fasex"){
    granulo="dia/cont/fasex/"
  }else if(fase == "fase0"){
    granulo = "dia/cont/fase0/"}
  else if (fase == "fase1"){
    granulo = "dia/cont/fase1/"
  }
  
  
  path = paste0("./",granulo)
  files = list.files(path)
  files = sort(files)
  filepath = paste0(path,files[number])
  return(filepath)
}

filePath1 <- function(number,fase){
  if (fase == "fasex"){
    granulo="dia/fasex/"
  }else if(fase == "fase0"){
    granulo = "dia/fase0/"}
  else if (fase == "fase1"){
    granulo = "dia/fase1/"
  }
  
  
  path = paste0("./",granulo)
  files = list.files(path)
  files = sort(files)
  filepath = paste0(path,files[number])
  return(filepath)
}


#filePath1(2,"fasex")
#filePathCont1(1,"fasex")


mediaTramosHora = function(){
  medias = data.frame()
  i = 1
  for (fichero in 1:24){
    medias[fichero,"hora"] = i
    i = i+1
    df = fileTable(filePath1(fichero,"fasex"))
    media = mean(as.numeric(df$ih),na.rm = T)
    medias[fichero,"Confinamiento"] = round(media) 
  }
  for (fichero in 1:24){
    df = fileTable(filePath1(fichero,"fase0"))
    media = mean(as.numeric(df$ih),na.rm = T)
    medias[fichero,"Fase_0"] = round(media) 
  }
  for (fichero in 1:24){
    df = fileTable(filePath1(fichero,"fase1"))
    media = mean(as.numeric(df$ih),na.rm = T)
    medias[fichero,"Fase_1"] = round(media) 
  }
  
  return(medias)
  
}
#suma = mediaTramosHora()


timeSeriesHora = function(df){
  cols <- c("Confinamiento"="red","Fase 0"="blue","Fase 1"="green")
  a = ggplot(df) +                    # basic graphical object
    geom_line(aes(x=df$hora, y=df$"Confinamiento", colour="Confinamiento"),cex = 1.2) +
    geom_line(aes(x=df$hora, y=df$"Fase_0", colour="Fase 0"),cex = 1.2) +
    geom_line(aes(x=df$hora, y=df$"Fase_1", colour="Fase 1"),cex = 1.2) +
    xlab("Día") + ylab("Media coches/día por tramo") + ggtitle("Promedio coches/hora por tramo")+
    scale_colour_manual("",values=cols)+
    scale_x_continuous(breaks=c(1, 2, 3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24),
                       labels=c("00:00","01:00","02:00","03:00","04:00","05:00",
                                "06:00","07:00","08:00","09:00","10:00","11:00","12:00",
                                "13:00","14:00","15:00","16:00","17:00","18:00","19:00",
                                "20:00","21:00","22:00","23:00"))+
    theme(axis.text=element_text(size=10),
          axis.title=element_text(size=12),
          plot.title = element_text(hjust = 0.5,size = 14)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
  
  return(a)
}
#timeSeriesHora(mediaTramosHora())

filePathCont1(23,"fase1")
mediaContHora = function(contaminante){
  medias = data.frame()
  i = 1
  for (fichero in 1:23){
    medias[fichero,"dia"] = i
    i = i+1
    df = fileTable(filePathCont1(fichero,"fasex"))
    media = mean(as.numeric(sub(",",".",df[,contaminante])),na.rm = T)
    medias[fichero,"Confinamiento"] = round(media,2) 
  }
  for (fichero in 1:23){
    df = fileTable(filePathCont1(fichero,"fase0"))
    media = mean(as.numeric(sub(",",".",df[,contaminante])),na.rm = T)
    medias[fichero,"Fase_0"] = round(media,2) 
  }
  for (fichero in 1:23){
    df = fileTable(filePathCont1(fichero,"fase1"))
    media = mean(as.numeric(sub(",",".",df[,contaminante])),na.rm = T)
    medias[fichero,"Fase_1"] = round(media,2) 
  }
  
  return(medias)
  
}
#suma = mediaContHora("no2")



timeSeriesContaminantesHoras = function(df,contaminante){
  y_lab = paste0(contaminante, " microgramos/m3")
  gg_title = paste0("Niveles: ",contaminante)
  cols <- c("Confinamiento"="red","Fase 0"="blue","Fase 1"="green")
  y2017 = as.numeric(df[,2])
  y2020 = df[,3]
  a = ggplot(df) +                    # basic graphical object
    geom_line(aes(x=df$dia, y=df$"Confinamiento", colour="Confinamiento"),cex = 1.2) +
    geom_line(aes(x=df$dia, y=df$"Fase_0", colour="Fase 0"),cex = 1.2) +
    geom_line(aes(x=df$dia, y=df$"Fase_1", colour="Fase 1"),cex = 1.2) +
    xlab("Día") + ylab(y_lab) + ggtitle(gg_title)+
    scale_colour_manual("",values=cols)+
    scale_x_continuous(breaks=c(1, 2, 3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24),
                       labels=c("00:00","01:00","02:00","03:00","04:00","05:00",
                                "06:00","07:00","08:00","09:00","10:00","11:00","12:00",
                                "13:00","14:00","15:00","16:00","17:00","18:00","19:00",
                                "20:00","21:00","22:00","23:00"))+
    theme(axis.text=element_text(size=10),
          axis.title=element_text(size=12),
          plot.title = element_text(hjust = 0.5,size = 14)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
  
  
  return(a)
  
}
