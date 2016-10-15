
# Archivo server.R
# @author         Cristian Gómez Alvarez <cristianpark@gmail.com>
# @contributor    Juan Camilo Cuartas <juancamilo.cuartas@gmail.com>
#
# Contiene las rutinas de los diferentes ajustes que se hacen a series de tiempo
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
library(forecast)

shinyServer(function(input, output){
  serie<-NULL
  serie.fit<-NULL
  serie.for<-NULL
  
  #Función para cargar el archivo de la serie a analizar
  cargarArchivo<-reactive({   
    if(is.null(serie)){
      #Leer los datos del archivo utilizando la configuración seleccionada (se necesita reactive para poner hacerlo todo)
      decimalSep<-input$decimalSep
      datosSep<-input$datosSep
      encabezado<-input$encabezado
      lineasSaltar<-input$lineasSaltar
      periodicidad<-input$periodicidad
      columnaDatos<-input$columnaDatos
      anioInicio<-input$anioInicio
      periodoInicio<-input$periodoInicio
      
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
      
      #Preview del archivo
      output$previewArchivo<-renderPrint({
        output$labelPreviewArchivo<-renderUI({ tags$b("Preview") })
        
        head(seriecsv, 3, addrownums = FALSE)
      })
      
      #Crea la serie de tiempo partiendo del CSV
      serie<-ts(data=seriecsv[[columnaDatos]],        #Genera los datos aleatorios
         start = c(as.numeric(anioInicio), as.numeric(periodoInicio)),         #Año de inicio - periodo de inicio
         frequency = as.numeric(periodicidad))
    }
      
    return(serie)
  })
  
  #Obtener los registros excepto los últimos  (para el conjunto de entrenamiento)
  primerosSerie<-function(data, n){
    data <- as.ts(data)
    as.ts(window(data,start=tsp(data)[1], end=tsp(data)[2]-n/frequency(data)))
  }
  
  #Obtener los últimos n periodos de una serie (para el conjunto de validación)
  ultimosSerie<- function(data,n) {
    data <- as.ts(data)
    as.ts(window(data,start=tsp(data)[2]-(n-1)/frequency(data)))
  }
  
  #Conjuntos de prueba/validación
  conjuntosPrueba<-reactive({
    serie<-cargarArchivo()
    
    if(!is.null(serie)){
      n<-length(serie) #longitud de la serie
      
      serie.fit<-primerosSerie(serie, as.numeric(input$periodosValidacion))
      serie.for<-ultimosSerie(serie, as.numeric(input$periodosValidacion))
    }
    
    #Vector con la serie y los conjuntos de prueba y validación
    return(list(serie=serie, fits=serie.fit, fors=serie.for))
  })
  
  ### Método de suavizado exponencial
  
  #Gráfico de la serie 
  output$graficoSuavizado<-renderPlot({
    serie<-cargarArchivo()
    if(!is.null(serie)){
      options(repr.plot.width=10, repr.plot.height=6)
      plot(serie)
      
      legend( "top",                                     # posicion
              c("Serie real"),         # texto
              lwd = c(2),                        # grosor lineas
              col = c('black'), # color lineas
              bty = "n")                                     # caja alrededor de la leyenda
    }
  })
  
  #Aplicación de método Holt-Winters
  output$holtWinters<-renderPrint({
    conjuntos<-conjuntosPrueba()
    
    if(!is.null(conjuntos$serie)){
      serie<-conjuntos$serie
      serie.fit<-as.ts(conjuntos$fits)
      serie.for<-as.ts(conjuntos$fors)
      
      #Debug
#       cat(file=stderr(), "For ", serie.for, "\n")
      
      modelo<-HoltWinters(x = serie.fit,      # conjunto de entrenamiento de la serie
                        alpha = NULL,    # NULL indica que se calcule el alpha óptimo
                        beta = FALSE,   # no se considera esta componente
                        gamma = FALSE)  # no se considera esta componente 
        
        #Error cuadrático
        output$errorWinters<-renderUI({tags$code(modelo$SSE)})
        
        #Dibujar el gráfico de ajuste
        output$graficoAjusteSE<-renderPlot({
          options(repr.plot.width=10, repr.plot.height=6)
          
          plot(serie,              # datos de la serie
               type = "o",     # o -- overplot
               lwd = 3,        # ancho de la linea
               ylim = c(min(serie)-1,max(serie)+1)) # límites min y max del eje Y
          
          lines(modelo$fitted[,1], col = "blue",lwd = 2)
          
          legend( "topleft",                                     # posicion
                  c("Serie real","modelo"),         # texto
                  lwd = c(3, 2),                        # grosor lineas
                  col = c('black','blue'), # color lineas
                  bty = "n")                                     # caja alrededor de la leyenda
          
          grid()
        })        
        
        #Predicción de los n periodos elegidos por el usuario
        prediccionHW <- predict( modelo,                         # Modelo ajustado por HW
               n.ahead = as.numeric(input$periodosPrediccion),                # Periodos a pronosticar
               prediction.interval = TRUE) # Calcular intervalos

        #Gráfico de la predicción
        output$graficoPrediccionSE<-renderPlot({
          options(repr.plot.width=10, repr.plot.height=6)
          
          plot(serie,              # datos de la serie
               type = "o",     # o -- overplot
               lwd = 3,        # ancho de la linea
               ylim = c(min(serie)-1,max(serie)+1)) # límites min y max del eje Y
          
          lines( prediccionHW[,1], col ="red",  lwd = 2)
          
          legend( x = "topleft", 
                  legend = c("Real", "modelo"),
                  lwd = c(3,2),
                  col = c('black','red'),
                  bty = 'n')
          
          grid()
        })
        
        #Predicción usando forecast
        prediccionFSE<-forecast.HoltWinters(modelo,   # Modelo ajustado por HW
                           h=as.numeric(input$periodosPrediccion))  # Periodos a pronosticar

        #Gráfico de la predicción
        output$graficoPrediccionFSE<-renderPlot({
          options(repr.plot.width=10, repr.plot.height=6)
          plot.forecast(prediccionFSE)
        })
        
        #Mostrar la información del método Holt-Winters
        modelo
    }
  })

  ### Suavizado doble
  
  #Gráfico de la serie 
  output$graficoSuavizadoD<-renderPlot({
    serieD<-cargarArchivo()
    if(!is.null(serieD)){
      options(repr.plot.width=10, repr.plot.height=6)
      plot(serieD)
      
      legend( "top",                                     # posicion
              c("Serie real"),         # texto
              lwd = c(2),                        # grosor lineas
              col = c('black'), # color lineas
              bty = "n")                                     # caja alrededor de la leyenda
    }
  })
  
  #Aplicación de método Holt-Winters
  output$holtWintersD<-renderPrint({
    conjuntos<-conjuntosPrueba()
    
    if(!is.null(conjuntos$serie)){
      serieD<-conjuntos$serie
      serieD.fit<-as.ts(conjuntos$fits)
      serieD.for<-as.ts(conjuntos$fors)
      
      #Debug
      #       cat(file=stderr(), "For ", serie.for, "\n")
      
      modeloD<-HoltWinters(x = serieD.fit,      # conjunto de entrenamiento de la serie
                          alpha = NULL,    # NULL indica que se calcule el alpha óptimo
                          beta = NULL,   # no se considera esta componente
                          gamma = FALSE)  # no se considera esta componente 
      
      #Error cuadrático
      output$errorWintersD<-renderUI({tags$code(modeloD$SSE)})
      
      #Dibujar el gráfico de ajuste
      output$graficoAjusteSED<-renderPlot({
        options(repr.plot.width=10, repr.plot.height=6)
        
        plot(serieD,              # datos de la serie
             type = "o",     # o -- overplot
             lwd = 3,        # ancho de la linea
             ylim = c(min(serieD)-1,max(serieD)+1)) # límites min y max del eje Y
        
        lines(modeloD$fitted[,1], col = "blue",lwd = 2)
        
        legend( "topleft",                                     # posicion
                c("Serie real","modelo"),         # texto
                lwd = c(3, 2),                        # grosor lineas
                col = c('black','blue'), # color lineas
                bty = "n")                                     # caja alrededor de la leyenda
        
        grid()
      })        
      
      #Predicción de los n periodos elegidos por el usuario
      prediccionHWD <- predict( modeloD,                         # Modelo ajustado por HW
                               n.ahead = as.numeric(input$periodosPrediccion),                # Periodos a pronosticar
                               prediction.interval = TRUE) # Calcular intervalos
      
      #Gráfico de la predicción
      output$graficoPrediccionSED<-renderPlot({
        options(repr.plot.width=10, repr.plot.height=6)
        
        plot(serieD,              # datos de la serie
             type = "o",     # o -- overplot
             lwd = 3,        # ancho de la linea
             ylim = c(min(serieD)-1,max(serieD)+1)) # límites min y max del eje Y
        
        lines( prediccionHWD[,1], col ="red",  lwd = 2)
        
        legend( x = "topleft", 
                legend = c("Real", "modelo"),
                lwd = c(3,2),
                col = c('black','red'),
                bty = 'n')
        
        grid()
      })
      
      #Predicción usando forecast
      prediccionFSED<-forecast.HoltWinters(modeloD,   # Modelo ajustado por HW
                                          h=as.numeric(input$periodosPrediccion))  # Periodos a pronosticar
      
      #Gráfico de la predicción
      output$graficoPrediccionFSED<-renderPlot({
        options(repr.plot.width=10, repr.plot.height=6)
        plot.forecast(prediccionFSED)
      })
      
      #Mostrar la información del método Holt-Winters
      modeloD
    }
  })
})