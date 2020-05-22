library(leaflet)
library(leaflet.extras)
require(ggplot2)
library(readxl)
library(corrplot)
library(caret)
library(ropls)
library(pls)
library(gridExtra)

filePath <- function(number,granulo,year){
  if (granulo == "horas"){
    granulo="horas/"
  }else{
    granulo = "dias/"}
  if (year == 2017){
    year = "2017/"
  }else{
    year= "2020/"}
  
  
  path = paste0("./",year,granulo)
  files = list.files(path)
  files = sort(files)
  filepath = paste0(path,files[number])
  return(filepath)
}
#filePath(1,"dias",2017)

fileTable <- function(path){
  dataframe = read.csv(
    file = path,
    stringsAsFactors = FALSE, 
    sep = ';',
    encoding = "UTF-8")
  dataframe = dataframe[complete.cases(dataframe), ]
  #if ("ih" %in% colnames(dataframe)){
  #  names(dataframe)[names(dataframe) == 'ih'] <- 'lectura'
  #}
  return(dataframe)
}
#z =fileTable(filePath(2,"dias",2017))

listarFicheros = function(granulo,year){
  if (granulo == "horas"){
    granulo="horas/"
  }else{
    granulo = "dias/"}
  if (year == 2017){
    year = "2017/"
  }else{
    year= "2020/"}
  path = paste0("./",year,granulo)
  files = list.files(path)
  return(sort(files))
  
}
#listarFicheros("dias",2017)

#fichero = 1
mediaTramosDia = function(){
  medias = data.frame()
  i = 1
  for (fichero in 1:length(listarFicheros("dias",2017))){
    medias[fichero,"dia"] = i
    i = i+1
    df = fileTable(filePath(fichero,"dias",2017))
    media = mean(as.numeric(df$ih),na.rm = T)
    medias[fichero,"2017"] = round(media) 
  }
  for (fichero in 1:length(listarFicheros("dias",2020))){
    df = fileTable(filePath(fichero,"dias",2020))
    media = mean(as.numeric(df$ih),na.rm = T)
    medias[fichero,"2020"] = round(media) 
  }
  return(medias)
  
}
#suma = mediaTramosDia()

traficoPS = function(){
  #Tráfico medio de pista de silla
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
  cols <- c("2017"="red","2020"="blue")
  a = ggplot(df) +                    # basic graphical object
    geom_line(aes(x=df$dia, y=df$"2017", colour="2017"),cex = 1.2) +
    geom_line(aes(x=df$dia, y=df$"2020", colour="2020"),cex = 1.2) +
    xlab("Día") + ylab("Media coches/dia por tramo") + ggtitle("Promedio coches/dia por tramo")+
    scale_colour_manual("Año",values=cols)+
    scale_x_continuous(breaks=c(1, 2, 3,4,5,6,7,8,9,10,11,12,13,14,15),
                     labels=c("Martes","Miercoles","Jueves","Viernes","Sábado",
                              "Domingo","Lunes","Martes","Miercoles","Jueves",
                              "Viernes","Sábado","Domingo","Lunes","Martes"))+
    theme(axis.text=element_text(size=10),
          axis.title=element_text(size=12),
          plot.title = element_text(hjust = 0.5,size = 14)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
    
  return(a)
}

contaminanteDia =  function(contaminante){
  if (contaminante == "so2"){
    c_2017 = "SO2"
    c_2020 = "so2"
  }else if(contaminante=="no2"){
    c_2017 = "NO2"
    c_2020 = "no2"
    
  }else if(contaminante=="o3"){
    c_2017 = "O3"
    c_2020 = "o3"
    
  }else if(contaminante=="pm10"){
    c_2017 = "PM10"
    c_2020 = "pm10"
  }else if(contaminante=="pm25"){
    c_2017 = "PM2.5"
    c_2020 = "pm25"
  }
  df = data.frame()
  d2017 = readxl::read_xlsx("./estacion/dia/estacion2017.xlsx")
  d2020 = readxl::read_xlsx("./estacion/dia/estacion2020.xlsx")
  df[1:15,"dia"] = 1:15
  df[1:15,paste0(contaminante,"-","2017")] = d2017[1:15,c_2017]
  df[1:15,paste0(contaminante,"-","2020")] = d2020[1:15,c_2020]
  
  return(df)  
}
#df = contaminanteDia("pm10")
# pm10 = microgramos/metro_cubico
# no2 =  microgramos/metro_cubico
# so2 =  microgramos/metro_cubico
# pm25 =  microgramos/metro_cubico
# o3 =  microgramos/metro_cubico

timeSeriesContaminantes = function(df){
  col_name = colnames(df)[2]
  contaminante = substr(col_name,1,nchar(col_name)-5)
  y_lab = paste0(contaminante, " µg/m3")
  gg_title = paste0("Niveles: ",contaminante)
  cols <- c("2017"="red","2020"="blue")
  y2017 = as.numeric(df[,2])
  y2020 = df[,3]
  a = ggplot(df) +                    # basic graphical object
    geom_line(aes(x=df$dia, y=y2017, colour="2017"),cex = 1.2) +
    geom_line(aes(x=df$dia, y=y2020, colour="2020"),cex = 1.2) +
    xlab("Día") + ylab(y_lab) + ggtitle(gg_title)+
    scale_colour_manual("Año",values=cols)+
    scale_x_continuous(breaks=c(1, 2, 3,4,5,6,7,8,9,10,11,12,13,14,15),
                       labels=c("Martes","Miercoles","Jueves","Viernes","Sábado",
                                "Domingo","Lunes","Martes","Miercoles","Jueves",
                                "Viernes","Sábado","Domingo","Lunes","Martes"))+
    theme(axis.text=element_text(size=10),
          axis.title=element_text(size=12),
          plot.title = element_text(hjust = 0.5,size = 14)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
  
  multiplicador = round((sum(y2017) / sum(y2020)) * 100,0)
  print(paste0("Los valores del ",contaminante," en 2017 son un ",multiplicador, "% los valores de 2020" ))
  return(a)
  
}
#timeSeriesContaminantes(contaminanteDia("pm10"))
# timeSeriesContaminantes(contaminanteDia("pm25"))
# timeSeriesContaminantes(contaminanteDia("no2"))
# timeSeriesContaminantes(contaminanteDia("so2"))
# timeSeriesContaminantes(contaminanteDia("o3"))

vientoVlc = function(){
  df = read.table("./vientoPorDia.csv",sep = ",",dec = ",",encoding = "UTF8",header = T)
  colnames(df)[1] <- gsub('^...','',colnames(df)[1])
  colnames(df)[2] = "2017"
  return(df)
}

tempVlc = function(){
  df = read.table("./tempPorDia.csv",sep = ",",dec = ",",encoding = "UTF8",header = T)
  colnames(df)[1] <- gsub('^...','',colnames(df)[1])
  colnames(df)[2] = "2017"
  return(df)
}
#tempVlc()

generarDatos = function(anyo){
  if (anyo == 2020){
  trafico = traficoPS()
  trafico = as.numeric(trafico$'2020')
  no2 = contaminanteDia("no2")
  no2 = as.numeric(no2$'no2-2020')
  
  pm10 = contaminanteDia("pm10")
  pm10 = as.numeric(pm10$'pm10-2020')
  
  pm25 = contaminanteDia("pm25")
  pm25 = as.numeric(pm25$'pm25-2020')
  
  so2 = contaminanteDia("so2")
  so2 = as.numeric(so2$'so2-2020')
  
  o3 = contaminanteDia("o3")
  o3 = as.numeric(o3$'o3-2020')
  
  viento = vientoVlc()
  viento = as.numeric(viento$"2020")
  # 
  #temp  = tempVlc()
  #temp = as.numeric(temp$"2020")
  }else{
    trafico = traficoPS()
    trafico = as.numeric(trafico$'2017')
    no2 = contaminanteDia("no2")
    no2 = as.numeric(no2$'no2-2017')
    
    pm10 = contaminanteDia("pm10")
    pm10 = as.numeric(pm10$'pm10-2017')
    
    pm25 = contaminanteDia("pm25")
    pm25 = as.numeric(pm25$'pm25-2017')
    
    so2 = contaminanteDia("so2")
    so2 = as.numeric(so2$'so2-2017')
    
    o3 = contaminanteDia("o3")
    o3 = as.numeric(o3$'o3-2017')
    
    viento = vientoVlc()
    viento$`2017`
    viento = as.numeric(viento$"2017")
    
  }
  
  datos = data.frame(trafico,no2,pm10,pm25,so2,o3,viento)
  datos = as.data.frame(datos)
  datos
}


#anyo = 2020
correlacionTraficoContaminantesViento = function(anyo){
  
  datos = generarDatos(anyo)
  corrplot::corrplot(cor(datos))
}



timeSeriesModelo = function(real,predicho,nombre,anyo){
  y_lab = paste0(nombre, " µg/m3")
  gg_title = paste0("Niveles: ",nombre, " ",anyo)
  cols <- c("Predicho"="red","Real"="blue")
  a = ggplot() +                    # basic graphical object
    geom_line(aes(x=1:15, y=real, colour="Real"),cex = 1.2) +
    geom_line(aes(x=1:15, y=predicho, colour="Predicho"),cex = 1.2) +
    xlab("Día") + ylab(y_lab) + ggtitle(gg_title)+
    scale_colour_manual("Datos",values=cols)+
    scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15),
                       labels=c("Martes","Miercoles","Jueves","Viernes","Sábado",
                                "Domingo","Lunes","Martes","Miercoles","Jueves",
                                "Viernes","Sábado","Domingo","Lunes","Martes"))+
    theme(axis.text=element_text(size=10),
          axis.title=element_text(size=12),
          plot.title = element_text(hjust = 0.5,size = 14),
          axis.text.x = element_text(angle = 45,hjust = 1)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
  return(a)}


trainModelo =  function(datos){
  caret::train(contaminante ~ trafico + viento^2,
                         method = "glmnet",data =datos)
}

modeloLinealSimple = function(anyo,nombre){
  datos = generarDatos(anyo)
  contaminante = datos[,nombre]
  trafico = datos$trafico
  viento = datos$viento
  
  regresion <- lm(contaminante ~ viento  + trafico)
  predicho = as.numeric(paste(unlist(fitted(regresion))))
  a = timeSeriesModelo(contaminante,predicho,nombre,anyo)
  
  b = summary(regresion)
  print(paste0("Modelo de regresión de ",anyo, " para ",nombre ))
  print(b)
  return(a)
}

plotear2graficas = function(a,b){
  grid.arrange(a,b, nrow = 1)
}



#modeloLinealSimple(2020,"no2")

# summary(lm_fit)
# 
# 
# #ANÁLISIS PLS
# 
# Y = datos[,2]
# X = datos[,c(1,7)]
# 
# Yso2 = datos[,5]
# Xso2 = datos[,c(1,2,3,4,6,7)]
# 
# mypls_ = opls(x = X, y = Y, crossvalI = nrow(X), scaleC = "standard")
# mypls_ = opls(x = X, y = Y, predI = 2, crossvalI = nrow(X),
#              scaleC = "standard")
# mypls_@modelDF
# plot(1:5, mypls_@modelDF$`R2Y(cum)`, type = "o", pch = 16, col = "blue3",
#      lwd = 2, xlab = "Components", ylab = "", ylim = c(0,0.9),
#      main = "Modelo PLS: No2")
# 
# lines(1:5, mypls_@modelDF$`Q2(cum)`, type = "o", pch = 16, col = "red3",
#       lwd = 2)
# 
# abline(h = 0.5, col = "red3", lty = 2)
# legend("bottomleft", c("R2", "Q2"), lwd = 2, 
#        col = c("blue3", "red3"), bty = "n")
# 
# mypls_@vipVn
# barplot(sort(mypls_@vipVn, decreasing = TRUE) , main = "VIP", las = 2)
# abline(h = 1, col = 2, lty = 2)
# 
# plot(x = mypls_, typeVc = "x-score", parCexN = 0.8, parCompVi = c(1, 2),
#      parAsColFcVn = NA,
#      parEllipsesL = TRUE, parLabVc = rownames(X), parPaletteVc = NA,
#      parTitleL = TRUE, parCexMetricN = NA, plotPhenoDataC = NA,
#      plotSubC = NA, fig.pdfC = NA, file.pdfC = NULL)
# 
# plot(x = mypls_, typeVc = "xy-weight",
#      parCexN = 0.8, parCompVi = c(1, 2),
#      parPaletteVc = NA,
#      parTitleL = TRUE, parCexMetricN = NA, plotPhenoDataC = NA,
#      plotSubC = NA  )
# 
# 
# plot(x = mypls_, typeVc = "x-loading",
#      parCexN = 0.8, parCompVi = c(1, 2),
#      parPaletteVc = NA,
#      parTitleL = TRUE, parCexMetricN = NA, plotPhenoDataC = NA,
#      plotSubC = NA, fig.pdfC = NA, file.pdfC = NULL)
# 
# plot(x = mypls_, typeVc = "outlier",
#      parCexN = 0.8, parCompVi = c(1, 2),
#      parPaletteVc = NA,
#      parTitleL = TRUE, parCexMetricN = NA, plotPhenoDataC = NA,
#      plotSubC = NA, fig.pdfC = NA, file.pdfC = NULL)
# 
# 
# par(mfrow = c(1,2))
# plot(mypls_@scoreMN[,1], mypls_@uMN[,1], xlab = "t", ylab = "u",
#      main = "Component 1")
# abline(a=0, b=1, col = "grey", lty = 3)
# plot(mypls_@scoreMN[,2], mypls_@uMN[,2], xlab = "t", ylab = "u",
#      main = "Component 2")
# abline(a=0, b=1, col = "grey", lty = 3)  
# 
# -----------------------------------------------------------------------------------------------------------
# 
# mypls_so2 = opls(x = Xso2, y = Yso2, crossvalI = nrow(Xso2), scaleC = "standard")
# mypls_so2 = opls(x = Xso2, y = Yso2, predI = 5, crossvalI = nrow(Xso2),
#               scaleC = "standard")
# mypls_@modelDF
# plot(1:5, mypls_so2@modelDF$`R2Y(cum)`, type = "o", pch = 16, col = "blue3",
#      lwd = 2, xlab = "Components", ylab = "", ylim = c(0,0.9),
#      main = "Modelo PLS: No2")
# 
# lines(1:5, mypls_so2@modelDF$`Q2(cum)`, type = "o", pch = 16, col = "red3",
#       lwd = 2)
# 
# abline(h = 0.5, col = "red3", lty = 2)
# legend("bottomleft", c("R2", "Q2"), lwd = 2, 
#        col = c("blue3", "red3"), bty = "n")
# 
# mypls_so2@vipVn
# barplot(sort(mypls_so2@vipVn, decreasing = TRUE) , main = "VIP", las = 2)
# abline(h = 1, col = 2, lty = 2)
# 
# plot(x = mypls_so2, typeVc = "x-score", parCexN = 0.8, parCompVi = c(1, 2),
#      parAsColFcVn = NA,
#      parEllipsesL = TRUE, parLabVc = rownames(Xso2), parPaletteVc = NA,
#      parTitleL = TRUE, parCexMetricN = NA, plotPhenoDataC = NA,
#      plotSubC = NA, fig.pdfC = NA, file.pdfC = NULL)
# 
# plot(x = mypls_so2, typeVc = "xy-weight",
#      parCexN = 0.8, parCompVi = c(1, 2),
#      parPaletteVc = NA,
#      parTitleL = TRUE, parCexMetricN = NA, plotPhenoDataC = NA,
#      plotSubC = NA, fig.pdfC = NA, file.pdfC = NULL)
# 
# 
# plot(x = mypls_, typeVc = "x-loading",
#      parCexN = 0.8, parCompVi = c(1, 2),
#      parPaletteVc = NA,
#      parTitleL = TRUE, parCexMetricN = NA, plotPhenoDataC = NA,
#      plotSubC = NA, fig.pdfC = NA, file.pdfC = NULL)
# 
# plot(x = mypls_, typeVc = "outlier",
#      parCexN = 0.8, parCompVi = c(1, 2),
#      parPaletteVc = NA,
#      parTitleL = TRUE, parCexMetricN = NA, plotPhenoDataC = NA,
#      plotSubC = NA, fig.pdfC = NA, file.pdfC = NULL)
# 
# 
# par(mfrow = c(1,2))
# plot(mypls_@scoreMN[,1], mypls_@uMN[,1], xlab = "t", ylab = "u",
#      main = "Component 1")
# abline(a=0, b=1, col = "grey", lty = 3)
# plot(mypls_@scoreMN[,2], mypls_@uMN[,2], xlab = "t", ylab = "u",
#      main = "Component 2")
# abline(a=0, b=1, col = "grey", lty = 3)


