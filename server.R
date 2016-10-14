
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)

shinyServer(function(input, output) {
  variable<<-1
  
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
      
    #hist(seriecsv$x)
    Dt  <- c(NaN, diff(seriecsv$x))                                           # cambio absoluto
    rt  <- c(NaN, log(seriecsv$x[2:length(seriecsv$x)] / seriecsv$x[1:(length(seriecsv$x) - 1)] )) # rentabilidad logaritmica 
      
    options(repr.plot.width=8, repr.plot.height=7)
    par(mfrow=c(3,1))
    
    plot.ts( seriecsv$x,  ylab = 'TRM', bty = 'n' );            grid()
    plot.ts( Dt,   ylab = 'Dt',  bty = 'n', col="blue"); grid()
    plot.ts( rt,   ylab = 'rt',  bty = 'n', col="red");  grid()
    
    variable<-5
    
       
    # generate bins based on input$bins from ui.R
    #
    #x    <- faithful[, 2] 
    #bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    #hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
  })
  
  output$normalidad<-renderText({
    print(variable)
    
#     options(repr.plot.width=8, repr.plot.height=7)
#     par(mfrow=c(1,1))
#     plot.ts( rt,   ylab = 'rt',  bty = 'n', col="red")
  })
  
  server <- function(input, output, session) {
    observeEvent(input$pruebaBoton, {
      output$salida<-renderText({ "Dieron click" })
      
      session$sendCustomMessage(type = 'testmessage',
                                message = 'Thank you for clicking')
    })
  }
})