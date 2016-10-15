
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)

shinyServer(function(input, output){
  seriecsv<-NULL
  
  #Función para cargar el archivo de la serie a analizar
  cargarArchivo<-reactive({    
    if(is.null(seriecsv)){
      #Leer los datos del archivo utilizando la configuración seleccionada (se necesita reactive para poner hacerlo todo)
      decimalSep<-input$decimalSep
      datosSep<-input$datosSep
      encabezado<-input$encabezado
      lineasSaltar<-input$lineasSaltar
      periodicidad<-input$periodicidad
      columnaDatos<-input$columnaDatos
      
      #Leer el archivo   
      archivo<-input$archivoSerie
      
      if (is.null(archivo))
        return(NULL)
      
      #Cargar el CSV
      seriecsv <- read.csv(file = archivo$datapath, # nombre del archivo
                           header = as.logical(encabezado),       # nombres de columnas
                           skip=as.numeric(lineasSaltar),
                           sep = as.character(datosSep),               # separador de campos
                           dec = as.character(decimalSep))               # separador de decimales
    }
      
    return(seriecsv)
  })
    
  #Gráfico de salida  
  output$distPlot <- renderPlot({
    seriecsv<-cargarArchivo()
    if(!is.null(seriecsv)){
      hist(seriecsv[[input$columnaDatos]])
    }
    
#     Dt  <- c(NaN, diff(seriecsv$x))                                           # cambio absoluto
#     rt  <- c(NaN, log(seriecsv$x[2:length(seriecsv$x)] / seriecsv$x[1:(length(seriecsv$x) - 1)] )) # rentabilidad logaritmica 
#       
#     options(repr.plot.width=8, repr.plot.height=7)
#     par(mfrow=c(3,1))
#     
#     plot.ts( seriecsv$x,  ylab = 'TRM', bty = 'n' );            grid()
#     plot.ts( Dt,   ylab = 'Dt',  bty = 'n', col="blue"); grid()
#     plot.ts( rt,   ylab = 'rt',  bty = 'n', col="red");  grid()  
  })
  
  #Gráfica de normalidad
  output$normalidad<-renderText({
    format(input$fechasSerie[1])
    format(input$fechasSerie[2])
  })
})