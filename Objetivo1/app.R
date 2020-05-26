library("shiny")
library("shinyWidgets")
library(leaflet)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
source("/usr/games/trafico/valencia-esp/plotearMapas.R",encoding = "UTF-8")

{ui <- dashboardPagePlus(skin ="blue", collapse_sidebar = T,
   title = "Tráfico de Valencia",
	
   header = dashboardHeaderPlus(enable_rightsidebar = F,title = "Tráfico de Valencia",titleWidth = "100%"
								 ),
  {dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Mapa", 
        tabName = "maps", 
        icon = icon("map-marked-alt")
        #menuSubItem("Watersheds", tabName = "m_water", icon = icon("map")),
        #menuSubItem("Population", tabName = "m_pop", icon = icon("map"))
      ),
      menuItem(
        "¿Cómo se ha hecho?", 
        tabName = "info", 
        icon = icon("info-circle")
        #menuSubItem("Watersheds", tabName = "c_water", icon = icon("area-chart")),
        #menuSubItem("Population", tabName = "c_pop", icon = icon("area-chart"))
      ),
      menuItem(
        "Contacto", 
        tabName = "contact", 
        icon = icon("info-circle")
        #menuSubItem("Watersheds", tabName = "c_water", icon = icon("area-chart")),
        #menuSubItem("Population", tabName = "c_pop", icon = icon("area-chart"))
      )
    )
  )},
  dashboardBody(useShinyjs(),
  	tags$head(tags$style(
      type = "text/css", 
      ".irs-grid-text {font-size: 12pt !important; transform: rotate(-60deg) translate(-30px);}
	   .irs-single {font-size: 20pt !important;transform: rotate(0deg) translate(0px,-30px);}
	   .leaflet-popup-content {width: 200px;}
	   text {font-size: 13pt}
	   h1 {!important;text-align:center;}
	   span {font-size: 13pt}
	   .leaflet-popup-content {font-size: 10pt;}
	   input { transform: scale(1.5); }
	   text {font-size: 14pt;}"
	   
    ),
	tags$link(rel = "shortcut icon", href = "icon.ico")),
    tabItems(
      {tabItem(
        #####
        
        ########### TAB DEL MAPA DE CALOR
        
        #####
        tabName = "maps",
        h1("Mapa de tráfico de Valencia en tiempo real"),
        collapsible = TRUE,
        width = "100%",
        height = "100%",
        box( background = "black",
             sliderTextInput(inputId = "date",HTML("<h3><strong>Elige la hora:</strong></h3></br>") , 
                             choices = c(getDate_Filename(filePath(1)), getDate_Filename(filePath(2)), getDate_Filename(filePath(3)),
                                         getDate_Filename(filePath(4)), getDate_Filename(filePath(5)), getDate_Filename(filePath(6)),
                                         getDate_Filename(filePath(7)), getDate_Filename(filePath(8)), getDate_Filename(filePath(9)),
                                         getDate_Filename(filePath(10)), getDate_Filename(filePath(11)), getDate_Filename(filePath(12)),
                                         getDate_Filename(filePath(13)), getDate_Filename(filePath(14)), getDate_Filename(filePath(15)),
                                         getDate_Filename(filePath(16)), getDate_Filename(filePath(17)), getDate_Filename(filePath(18)),
                                         getDate_Filename(filePath(19)), getDate_Filename(filePath(20))),
							 hide_min_max = T,
                             selected = c(getDate_Filename(filePath(20))),
                             grid = TRUE, 
                             force_edges = TRUE,
                             width = "100%"),
			width = "100%",
			br(),
			br(),
			br()
        ),
        leafletOutput("mymap",height=700)
      )},
      
      
      ##########
      
      ############### TAB DE INFO
      
      ##########
      tabItem(
        tabName = "info",
        includeHTML("/usr/games/trafico/valencia-esp/info.html") 
      ),
      tabItem(
        tabName = "contact",
        includeHTML("/usr/games/trafico/valencia-esp/contact.html") 
      )
    )
  )
)
  
  
  
}

{server <- function(input, output, session) {
	onevent("mouseenter", "sidebarCollapsed", shinyjs::removeCssClass(selector = "body", class = "sidebar-collapse"))
    onevent("mouseleave", "sidebarCollapsed", shinyjs::addCssClass(selector = "body", class = "sidebar-collapse"))
  observe({
    
    
    isolate({
      zoom = input$mymap_zoom
      lng = input$mymap_center$lng
      lat = input$mymap_center$lat
    })
    isolate({currentGroup = input$mymap_groups})
    
    
    value = input$date
    if (value== getDate_Filename(filePath(1))) {
      numero_fichero = 1
      
    }else if(value== getDate_Filename(filePath(2))){
      numero_fichero = 2
      
    }else if(value== getDate_Filename(filePath(3))){
      numero_fichero = 3
      
    }else if(value== getDate_Filename(filePath(4))){
      numero_fichero = 4
      
    }else if(value== getDate_Filename(filePath(5))){
      numero_fichero = 5
      
    }else if(value== getDate_Filename(filePath(6))){
      numero_fichero = 6
      
    }else if(value== getDate_Filename(filePath(7))){
      numero_fichero = 7
      
    }else if(value== getDate_Filename(filePath(8))){
      numero_fichero = 8
      
    }else if(value== getDate_Filename(filePath(9))){
      numero_fichero = 9
    }
    else if(value== getDate_Filename(filePath(10))){
      numero_fichero = 10
    }
    else if(value== getDate_Filename(filePath(11))){
      numero_fichero = 11
    }
    else if(value== getDate_Filename(filePath(12))){
      numero_fichero = 12
    }
    else if(value== getDate_Filename(filePath(13))){
      numero_fichero = 13
    }
    else if(value== getDate_Filename(filePath(14))){
      numero_fichero = 14
    }
    else if(value== getDate_Filename(filePath(15))){
      numero_fichero = 15
    }
    else if(value== getDate_Filename(filePath(16))){
      numero_fichero = 16
    }
    else if(value== getDate_Filename(filePath(17))){
      numero_fichero = 17
    }
    else if(value== getDate_Filename(filePath(18))){
      numero_fichero = 18
    }
    else if(value== getDate_Filename(filePath(19))){
      numero_fichero = 19
    }
    else if(value== getDate_Filename(filePath(20))){
      numero_fichero = 20
    }
    
    output$mymap <- renderLeaflet({
      plotearMapa(numero_fichero,lng,lat,zoom,currentGroup)
    })
    
    
    updateSliderTextInput(
      session = session,
      inputId = "date",
      choices = c(getDate_Filename(filePath(1)), getDate_Filename(filePath(2)), getDate_Filename(filePath(3)),
                  getDate_Filename(filePath(4)), getDate_Filename(filePath(5)), getDate_Filename(filePath(6)),
                  getDate_Filename(filePath(7)), getDate_Filename(filePath(8)), getDate_Filename(filePath(9)),
                  getDate_Filename(filePath(10)), getDate_Filename(filePath(11)), getDate_Filename(filePath(12)),
                  getDate_Filename(filePath(13)), getDate_Filename(filePath(14)), getDate_Filename(filePath(15)),
                  getDate_Filename(filePath(16)), getDate_Filename(filePath(17)), getDate_Filename(filePath(18)),
                  getDate_Filename(filePath(19)), getDate_Filename(filePath(20))))
  })
  
}
}

shinyApp(ui, server)
