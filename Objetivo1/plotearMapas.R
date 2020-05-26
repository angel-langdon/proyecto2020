library(leaflet) # for interactive maps
library(leaflet.extras)
library(htmlwidgets)

filePath <- function(numero){
    path = "/usr/games/trafico/valencia-esp/trafico/"
    files = list.files(path)
    files = sort(files)
    filepath = paste0(path,files[numero])
  return(filepath)
}

filePathEstacion <- function(numero){
  path = "/usr/games/trafico/valencia-esp/estacion/"
  files = list.files(path)
  files = sort(files)
  filepath = paste0(path,files[numero])
  return(filepath)
}

getDateEstacion = function(path){
  path = strsplit(path,"/")
  path = path[[1]][7]
  date = substr(path,10,nchar(path)-4)

  parte1 = substr(date,1,10)
  parte2 = substr(date,12,nchar(date))
  parte2 = gsub("-",":",parte2)
  mes = substr(parte1,1,2)
  dia = substr(parte1,4,5)
  year = substr(parte1,7,10)
  parte1 = paste(dia,mes,year,sep="-")
  date = paste(parte2,parte1,sep=" ")
  return(date)
}
#getDateEstacion(filePathEstacion(1))

fileTable <- function(path,estacion=FALSE){
  dataframe = read.csv(
    file = path,
    stringsAsFactors = FALSE, 
    sep = ';',
    encoding = "UTF-8")
  if (!estacion){
  dataframe = dataframe[complete.cases(dataframe), ]
  dataframe = dataframe[dataframe$ih != -1,]
  dataframe = dataframe[dataframe$ih <= 6250,]
  }else{
    for (row in 1:nrow(dataframe)){
      dataframe[row,14] = getDateEstacion(path)
    }
    colnames(dataframe)[14] <- 'fecha'
    drops <- c("co")
    dataframe = dataframe[ , !(names(dataframe) %in% drops)]
  }
  return(dataframe)
  }
#d = fileTable(filePathEstacion(1),estacion = T)

getDate_Filename = function(path){
  path = strsplit(path,"/")
  path = path[[1]][7]
  date = substr(path,9,nchar(path)-4)
  parte1 = substr(date,1,10)
  parte2 = substr(date,12,nchar(date))
  parte2 = gsub("-",":",parte2)
  mes = substr(parte1,6,7)
  dia = substr(parte1,9,10)
  year = substr(parte1,1,4)
  parte1 = paste(dia,mes,year,sep="-")
  #date = paste(parte2,parte1,sep=" ")
  date = parte2
  return(date)
}
#dataframe = fileTable(filePath(1))

#getDate_Filename("/usr/games/trafico/valencia-esp/trafico")

limpiarEstacionesHeatmap = function(dataEstacion){
  estacionHeatmap = subset(dataEstacion, dataEstacion$calidad_ambiental != 'Error')
  for (row in 1:nrow(estacionHeatmap)){
    calidad = estacionHeatmap[row,"calidad_ambiental"]
    if (calidad == "Bueno"){
      numero = 1
    }else if (calidad == "Aceptable"){
      numero = 2
    }else{
      numero = 3
    }
    estacionHeatmap[row,"calidad_numerica"] = numero
	}
	return(estacionHeatmap)
}



plotearMapa <- function(number,lngInput,latInput,zoomInput,currentGroup){
  if (is.null(lngInput)||is.null(latInput)||is.null(zoomInput)||is.null(currentGroup)){
    lngInput =-0.375328
    latInput = 39.474014
    zoomInput =  13
    currentGroup = c("Tráfico","E. Contaminación Atmosféricas","Calidad Ambiental")
  }
  dataEstacion = fileTable(filePathEstacion(2),estacion = TRUE)
  dataframe = fileTable(filePath(number))

  {
    
    #campos = c("idmp","ih")
    #colnames(dataframe)[6],
    #                  colnames(dataframe)[10],colnames(dataframe)[5],colnames(dataframe)[4],
    #                  colnames(dataframe)[8],colnames(dataframe)[2],colnames(dataframe)[9],
    #                  colnames(dataframe)[7],colnames(dataframe)[14])

    for (i in 1:nrow(dataframe)){
      valor = dataframe[i,"idpm"]
      valor2 = dataframe[i,"ih"]

      a = list(#"<b>Sensor</b>" = valor,
        "<b>Coches/hora</b>" = valor2)

      lista_elementos =strsplit(toString(paste(names(a),as.character(a),sep=":"),),split=",")

      content <- paste(sep = "<br/>",lista_elementos
      )
      content = character()
      for (elemento in lista_elementos){
        content = paste(sep = "<br/>",content,elemento)
      }
      content =gsub(",","",toString(content,sep=""))
      content

      content=substr(content,6,  nchar(content))
      dataframe$contenidos[i] = content
    }
    
    
    for (i in 1:nrow(dataEstacion)){
      valor = dataEstacion[i,"nombre"]
      valor2 = dataEstacion[i,"direccion"]
      valor3 = dataEstacion[i,"tipozona"]
      valor4 = dataEstacion[i,"tipoemision"]
      # valor5 = dataEstacion[i,"so2"]
      # valor6 = dataEstacion[i,"no2"]
      # valor7 = dataEstacion[i,"o3"]
      # valor8 = dataEstacion[i,"pm10"]
      # valor9 = dataEstacion[i,"pm25"]
      valor10 = dataEstacion[i,"calidad_ambiental"]
      valor11 = dataEstacion[i,"fecha"]
      
      a = list("<b>Nombre Estación</b>" = valor,
               "<b>Calidad ambiental</b>"=valor10,
               "Dirección" = valor2,
               "Tipo de zona"=valor3,"Tipo emisión"=valor4,
               #"SO2"=valor5,"NO2"=valor6,
               #"O3"=valor7,"PM10"=valor8,
               #"PM25"=valor9,
               "<b>Última actualización</b>"=valor11)
      
      lista_elementos =strsplit(toString(paste(names(a),as.character(a),sep=":"),),split=",")
      
      content <- paste(sep = "<br/>",lista_elementos
      )
      content = character()
      for (elemento in lista_elementos){
        content = paste(sep = "<br/>",content,elemento)
      }
      content =gsub(",","",toString(content,sep=""))
      
      content=substr(content,6,  nchar(content))
      dataEstacion$contenidos[i] = content
    }
  }
  
  cityIcon <- makeIcon(
    iconUrl = "/usr/games/trafico/valencia-esp/sensorIcon.png",
    iconWidth = 9, iconHeight = 9,
    iconAnchorX = 6, iconAnchorY = 6,
  )
  
  estationIcon <- makeIcon(
    iconUrl = "/usr/games/trafico/valencia-esp/estacionIcon.png",
    iconWidth = 30, iconHeight = 30,
    iconAnchorX = 6, iconAnchorY = 6,
  )
  
  t_col <- function(color, percent = 50, name = NULL) {
    #      color = color name
    #    percent = % transparency
    #       name = an optional name for the color
    
    ## Get RGB values for named color
    rgb.val <- col2rgb(color)
    
    ## Make new color using input color as base and alpha set by transparency
    t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
                 max = 255,
                 alpha = (100 - percent) * 255 / 100,
                 names = name)
    
    ## Save the color
    invisible(t.col)
  }
  
 lt.blue=  t_col("blue", percent = 90,name= "lt.blue")
 lt.cyan=  t_col("cyan", percent = 40,name= "lt.cyan")
 lt.blueO=  t_col("blue", percent = 40,name= "lt.blueO")
 lt.lime=  t_col("limegreen", percent = 30,name= "lt.lime")
 lt.yellow=  t_col("yellow", percent = 20,name= "lt.yellow")
 lt.red=  t_col("red", percent = 0,name= "lt.red")
  
  pal = function(data) {return(colorNumeric(
    # objetivo: 0.4 blue, 0.6 cyan, 0.7 lime,  0.8 yellow, 1 red
    # regular "#000066","#009999","#85b200","#cccc00","red"
    # mejor   "#0000FF","#00FFFF","#00FF00","#FFFF00","red"
    #lt.blue,lt.blueO,lt.cyan,
    #lt.blue,lt.blueO,lt.cyan,lt.lime,lt.yellow,lt.red

    palette = c("#00ff00","#00ff00","#ffff00","#ffff00","#FF0000"),
    domain = data,alpha = F
  ))}
  
  
  pal_2 = function(data) {return(colorNumeric(
    # objetivo: 0.4 blue, 0.6 cyan, 0.7 lime,  0.8 yellow, 1 red
    # regular "#000066","#009999","#85b200","#cccc00","red"
    # mejor   "#0000FF","#00FFFF","#00FF00","#FFFF00","red"
    #lt.blue,lt.blueO,lt.cyan,
    #lt.blue,lt.blueO,lt.cyan,lt.lime,lt.yellow,lt.red
    
    palette = c("#f0deff","#c075ff","#55009c"),
    domain = data,alpha = F
  ))}
  
  

  map = leaflet(dataframe) %>%
    addTiles(group = "Mapa", options = providerTileOptions(minZoom = 13)) %>%  # Add default OpenStreetMap map tiles
    setView(lng = lngInput,lat = latInput,zoom = zoomInput ) %>%
	setMaxBounds( lng1 = -0.4511702 , lat1 = 39.5182615
				, lng2 = -0.253045 , lat2 = 39.3983048 )
  #-3.812,40.275 , 5.3
  {
    {
    # porcentaje_var = grepl("delitosCada1000habitantes", filepath)
    # if (porcentaje_var){
    #   text1 = paste("Hurtos cada 1000 hab.<br>","Promedio: ",round(sum(dataframe$Hurtos)/244,digits = 4), " hurtos",sep="")
    #   text2 = paste("Violaciones cada 1000 hab.<br>","Promedio: ",round(sum(dataframe$Agresión.sexual.con.penetración)/244,digits = 4), " violaciones",sep="")
    #   text3 = paste("Robos de vehículo cada 1000 hab.<br>","Promedio: ",round(sum(dataframe$Sustracciones.de.vehículos)/244,digits = 4), " robos de vehículo",sep="")
    #   text4 = paste("Asesinatos dolosos cada 1000 hab.<br>","Promedio: ",round(sum(dataframe$Homicidios.dolosos.y.asesinatos.consumados)/244,digits = 4), " asesinatos dolosos",sep="")
    #   text5 = paste("delitos de Tráfico de droga cada 1000 hab.<br>","Promedio: ",round(sum(dataframe$Tráfico.de.drogas)/244,digits = 4), " delitos tráfico de drogas",sep="")
    #   text6 = paste("Secuestros cada 1000 hab.<br>","Promedio: ",round(sum(dataframe$Secuestro)/244,digits = 4), " secuestros",sep="")
    #   text7 = paste("Robos con violencia cada 1000 hab.<br>","Promedio: ",round(sum(dataframe$Robos.con.violencia.e.intimidación)/244,digits = 4), " robos con violencia",sep="")
    #   text8 = paste("Robos casas/lugares cada 1000 hab.<br>","Promedio: ",round(sum(dataframe$Robos.con.fuerza.en.domicilios..establecimientos.y.otras.instalaciones)/244,digits = 4), " robos casas/lugares",sep="")
    #   text9 = paste("Delitos totales cada 1000 hab.<br>","Promedio: ",round(sum(dataframe$Total)/244,digits = 4), " delitos totales",sep="")
    #   
    # }else{
    #   text1 = paste("Cantidad de Hurtos<br>","Total: ",sum(dataframe$Hurtos), " hurtos",sep="")
    #   text2 = paste("Cantidad de Violaciones<br>","Total: ",sum(dataframe$Agresión.sexual.con.penetración), " violaciones",sep="")
    #   text3 = paste("Cantidad de Robos de vehículo<br>","Total: ",sum(dataframe$Sustracciones.de.vehículos), " robos de vehículo",sep="")
    #   text4 = paste("Cantidad de Asesinatos dolosos<br>","Total: ",sum(dataframe$Homicidios.dolosos.y.asesinatos.consumados), " asesinatos dolosos",sep="")
    #   text5 = paste("Cantidad de delitos de Tráfico de drogas<br>","Total: ",sum(dataframe$Tráfico.de.drogas), " delitos tráfico de drogas",sep="")
    #   text6 = paste("Cantidad de Secuestros<br>","Total: ",sum(dataframe$Secuestro), " secuestros",sep="")
    #   text7 = paste("Cantidad de Robos con violencia<br>","Total: ",sum(dataframe$Robos.con.violencia.e.intimidación), " robos con violencia",sep="")
    #   text8 = paste("Cantidad de Robos casas/lugares<br>","Total: ",sum(dataframe$Robos.con.fuerza.en.domicilios..establecimientos.y.otras.instalaciones), " robos casas/lugares",sep="")
    #   text9 = paste("Cantidad de Delitos totales<br>","Total: ",sum(dataframe$Total), " delitos totales",sep="")
    # }
    }
    gradient = c("#00ff00","#00ff00","#ffff00","#ffff00","#FF0000")

    map = map %>%
      addHeatmap(lng=~Y, lat=~X,intensity = ~ih,
                 blur =10, max = 3500, radius =15, 
                 layerId = "Tráfico",group = "Tráfico",gradient = gradient)%>%
      addLegend("topleft", 
                pal =pal(0:3000), 
                values = 0:3000,
                title = "Leyenda: Tráfico",
                #labFormat = labelFormat(suffix = " Coches/hora"),
                opacity = 1,
                layerId = "Tráfico",
                group = "Tráfico",
                bins = 10,
                labFormat = function(type, cuts, p) {
                n = length(cuts)
                cuts[n] = "     Muy Denso" 
                for (i in 2:(n-1)){
                    cuts[i] = ""
                }
                cuts[1] = "     Fluido"
                #cuts[n %/% 4] = " Fluido"
                cuts[(n %/% 2)+1] = "     Denso"
                #cuts[(n * 3) %/% 4] = " Denso"
                cuts}
                )
    #dataEstacion
    gradient_2 = c("#f0deff","#c075ff","#55009c")
    estacionHeatmap = limpiarEstacionesHeatmap(dataEstacion)
    map = map %>%
      addHeatmap(lng=estacionHeatmap$Y, lat=estacionHeatmap$X,intensity = estacionHeatmap$calidad_numerica,
                 blur =30, max = 3, radius = 100, 
                 layerId = "Calidad Ambiental",group = "Calidad Ambiental",gradient = gradient_2,)%>%
      addLegend("topleft", 
                pal =pal_2(0:3), 
                values = 0:3,
                title = "Leyenda: Calidad Ambiental",
                #labFormat = labelFormat(suffix = " Coches/hora"),
                opacity = 1,
                layerId = "Calidad Ambiental",
                group = "Calidad Ambiental",
                bins = 10,
                labFormat = function(type, cuts, p) {
                  n = length(cuts)
                  cuts[n] = "     Deficiente" 
                  for (i in 2:(n-1)){
                    cuts[i] = ""
                  }
                  cuts[1] = "     Buena"
                  #cuts[n %/% 4] = " Fluido"
                  cuts[(n %/% 2)+1] = "     Aceptable"
                  #cuts[(n * 3) %/% 4] = " Denso"
                  cuts}
      )
    {
    # 
    # indicador = dataframe$Agresión.sexual.con.penetración
    # map = map %>%
    #   addHeatmap(lng=~longitudes, lat=~latitud,intensity = indicador,
    #              blur =1, max = max(indicador), radius =40,
    #              layerId = "Violacion",group = "Violacion")%>%
    #   addLegend("topleft", 
    #             pal = pal(dataframe$Agresión.sexual.con.penetración), 
    #             values = dataframe$Agresión.sexual.con.penetración,
    #             title = text2,
    #             labFormat = labelFormat(suffix = " violaciones"),opacity = 1,
    #             layerId = "Violacion",
    #             group = "Violacion")
    # 
    # indicador = dataframe$Sustracciones.de.vehículos
    # map = map %>%
    #   addHeatmap(lng=~longitudes, lat=~latitud,intensity = indicador,
    #              blur =1, max = max(indicador), radius =40,
    #              layerId = "Robo de vehículo",group = "Robo de vehículo")%>%
    #   addLegend("topleft", 
    #             pal = pal(dataframe$Sustracciones.de.vehículos), 
    #             values = dataframe$Sustracciones.de.vehículos,
    #             title = text3,
    #             labFormat = labelFormat(suffix = " robos de vehículo"),opacity = 1,
    #             layerId = "Robo de vehículo",
    #             group = "Robo de vehículo")
    # 
    # indicador = dataframe$Homicidios.dolosos.y.asesinatos.consumados
    # map = map %>%
    #   addHeatmap(lng=~longitudes, lat=~latitud,intensity = indicador,
    #              blur =1, max = max(indicador), radius =40,
    #              layerId = "Asesinatos dolosos",group = "Asesinatos dolosos")%>%
    #   addLegend("topleft", 
    #             pal = pal(dataframe$Homicidios.dolosos.y.asesinatos.consumados), 
    #             values = dataframe$Homicidios.dolosos.y.asesinatos.consumados,
    #             title = text4,
    #             labFormat = labelFormat(suffix = " asesinatos dolosos"),opacity = 1,
    #             layerId = "Asesinatos dolosos",
    #             group = "Asesinatos dolosos")
    # 
    # 
    # indicador = dataframe$Tráfico.de.drogas
    # map = map %>%
    #   addHeatmap(lng=~longitudes, lat=~latitud,intensity = indicador,
    #              blur =1, max = max(indicador), radius =40,
    #              layerId = "Tráfico de drogas",group = "Tráfico de drogas")%>%
    #   addLegend("topleft", 
    #             pal = pal(dataframe$Tráfico.de.drogas), 
    #             values = dataframe$Tráfico.de.drogas,
    #             title = text5,
    #             labFormat = labelFormat(suffix = " delitos tráfico de drogas"),opacity = 1,
    #             layerId = "Tráfico de drogas",
    #             group = "Tráfico de drogas")
    # 
    # indicador = dataframe$Secuestro
    # map = map %>%
    #   addHeatmap(lng=~longitudes, lat=~latitud,intensity = indicador,
    #              blur =1, max = max(indicador), radius =40,
    #              layerId = "Secuestro",group = "Secuestro")%>%
    #   addLegend("topleft", 
    #             pal = pal(dataframe$Secuestro), 
    #             values = dataframe$Secuestro,
    #             title = text6,
    #             labFormat = labelFormat(suffix = " secuestros"),opacity = 1,
    #             layerId = "Secuestro",
    #             group = "Secuestro")
    # 
    # indicador = dataframe$Robos.con.violencia.e.intimidación
    # map = map %>%
    #   addHeatmap(lng=~longitudes, lat=~latitud,intensity = indicador,
    #              blur =1, max = max(indicador), radius =40,
    #              layerId = "Robo con violencia",group = "Robo con violencia")%>%
    #   addLegend("topleft", 
    #             pal = pal(dataframe$Robos.con.violencia.e.intimidación), 
    #             values = dataframe$Robos.con.violencia.e.intimidación,
    #             title = text7,
    #             labFormat = labelFormat(suffix = " robos con violencia"),opacity = 1,
    #             layerId = "Robo con violencia",
    #             group = "Robo con violencia")
    # 
    # 
    # indicador = dataframe$Robos.con.fuerza.en.domicilios..establecimientos.y.otras.instalaciones
    # map = map %>%
    #   addHeatmap(lng=~longitudes, lat=~latitud,intensity = indicador,
    #              blur =1, max = max(indicador), radius =40,
    #              layerId = "Robos casas/lugares",group = "Robos casas/lugares")%>%
    #   addLegend("topleft", 
    #             pal = pal(dataframe$Robos.con.fuerza.en.domicilios..establecimientos.y.otras.instalaciones), 
    #             values = dataframe$Robos.con.fuerza.en.domicilios..establecimientos.y.otras.instalaciones,
    #             title = text8,
    #             labFormat = labelFormat(suffix = " robos casas/lugares"),opacity = 1,
    #             layerId = "Robos casas/lugares",
    #             group = "Robos casas/lugares")
    # 
    # indicador = dataframe$Total
    # map = map %>%
    #   addHeatmap(lng=~longitudes, lat=~latitud,intensity = indicador,
    #              blur =1, max = max(indicador), radius =40,
    #              layerId = "Total",group = "Total") %>%
    #   addLegend("topleft", 
    #             pal = pal(dataframe$Total), 
    #             values = dataframe$Total,
    #             title = text9,
    #             labFormat = labelFormat(suffix = " delitos totales"),opacity = 1,
    #             layerId = "Total",
    #             group = "Total")
      }
    # addLegend("bottomright", pal = mypal, values = df$values, 
    #           title = "quant data title", opacity = 1 
    #           ) 
    }

  map = map %>%  addMarkers(~Y,~X, icon = cityIcon,group ="Lecturas tráfico",popup = ~contenidos) %>%
    addMarkers(dataEstacion$Y,dataEstacion$X, icon = estationIcon,group ="E. Contaminación Atmosféricas",popup = dataEstacion$contenidos) %>%
    addLayersControl(
      overlayGroups = c("Tráfico" ,"Lecturas tráfico","E. Contaminación Atmosféricas","Calidad Ambiental" ),
      options = layersControlOptions(collapsed = F)) %>%
    hideGroup(c("Tráfico","Lecturas tráfico","E. Contaminación Atmosféricas","Calidad Ambiental")) %>%
    showGroup(currentGroup)
  return(map)
}

plotearMapa(number =18,NULL,NULL,NULL,NULL)
 
