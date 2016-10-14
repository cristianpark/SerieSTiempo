
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)

shinyServer(function(input, output) {
  #Mostrar la salida  
#   output$salida<-renderText({ print(cat("Salida ",decimalSep)) })  
  
  #serie1csv <- read.csv(file = 'serie1.csv', # nombre del archivo
  #                      header = TRUE,           # nombres de columnas
  #                      sep = " ",               # separador de campos
  #                      dec = ".")               # separador de decimales
  
  #output$salida<-renderText({ cat("Hola pues ", input$decimalSep) })
  
  output$distPlot <- renderPlot({
      #Leer los datos del archivo utilizando la configuración seleccionada  
      decimalSep<-input$decimalSep
      datosSep<-input$datosSep
      encabezado<-input$encabezado
      if(encabezado==TRUE)
        lineasSaltar<-0
      else
        lineasSaltar<-input$lineasSaltar
      periodicidad<-input$periodicidad
      columnaDatos<-input$columnaDatos
      
      #Leer el archivo    
      archivo<-input$archivoSerie
      
      if (is.null(archivo))
        return(NULL)
      
      seriecsv <- read.csv(file = archivo$datapath, # nombre del archivo
                            header = as.logical(encabezado),       # nombres de columnas
                            skip=as.numeric(lineasSaltar),
                            sep = as.character(datosSep),               # separador de campos
                            dec = as.character(decimalSep))               # separador de decimales
      x <- ts(data  = seriecsv,
              freq  = as.character(periodicidad),
              start = c(2010,1))

  })
  
})
