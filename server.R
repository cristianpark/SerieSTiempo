
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
  seriePronostico<-NULL
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
<<<<<<< HEAD
  
  
  output$graficoRegresion<-renderPlot({
    serie<-cargarArchivo()
    if(!is.null(serie)){
      options(repr.plot.width=10, repr.plot.height=6)
      ## Variable x
      t<- seq(1:length(serie))                          # Variable independiente t: Tiempo
      
      ## Ajuste
      m1 <- lm(serie~t)
      plot(t)
=======

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
>>>>>>> 635ee8ea1c71fc947eb091bb37f151ff99eb3555
    }
  })
  
  #Aplicación de método Holt-Winters
<<<<<<< HEAD
  output$resumenRegresion<-renderPrint({
    
    serie<-cargarArchivo()
    if(!is.null(serie)){
      options(repr.plot.width=10, repr.plot.height=6)
      ## Variable x
      t<- seq(1:length(serie))                          # Variable independiente t: Tiempo
      
      ## Ajuste
      m1 <- lm(serie~t)
        
      output$resumenRegresion<-renderPrint ({summary(m1)})
      
    }
  })
  
  output$graficoAjuste<-renderPlot({
    serie<-cargarArchivo()
    if(!is.null(serie)){
      options(repr.plot.width=10, repr.plot.height=6)
      ## Variable x
      t<- seq(1:length(serie))                          # Variable independiente t: Tiempo
      
      ## Ajuste
      m1 <- lm(serie~t)
      
      ## Grafica de ajuste.
      plot(t,serie, type = "o", lwd = 3)                                         # ancho de la linea
      
      
      lines(m1$fitted.values, col = "red", lwd = 2)
      
      
      legend( "topleft",                                     # posicion
              c("Serie real","Ajuste Regresion"),            # texto
              lwd = c(3, 2),                                 # grosor lineas
              col = c('black','red'),                        # color lineas
              bty = "n")                                     # sin caja alrededor de la leyenda
      
      grid()
      
      
      
    }
  })
  
=======
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
<<<<<<< HEAD
  #Fin suavizado doble

  ### Regresión lineal simple
  output$graficoRegresion<-renderPlot({
    serie<-cargarArchivo()
    if(!is.null(serie)){
      options(repr.plot.width=10, repr.plot.height=6)
      ## Variable x
      t<- seq(1:length(serie))                          # Variable independiente t: Tiempo
      
      ## Ajuste
      m1 <- lm(serie~t)
      plot(serie)
    }
  })

  output$resumenRegresion<-renderPrint({
    
    serie<-cargarArchivo()
    if(!is.null(serie)){
      options(repr.plot.width=10, repr.plot.height=6)
      ## Variable x
      t<- seq(1:length(serie))                          # Variable independiente t: Tiempo
      
      ## Ajuste
      m1 <- lm(serie~t)
      
      output$resumenRegresion<-renderPrint ({summary(m1)})
      
    }
  })
  
  output$graficoAjuste<-renderPlot({
    serie<-cargarArchivo()
    if(!is.null(serie)){
      options(repr.plot.width=10, repr.plot.height=6)
      ## Variable x
      t<- seq(1:length(serie))                          # Variable independiente t: Tiempo
      
      ## Ajuste
      m1 <- lm(serie~t)
      
      ## Grafica de ajuste.
      plot(t,serie, type = "o", lwd = 3)                                         # ancho de la linea
      
      
      lines(m1$fitted.values, col = "red", lwd = 2)
      
      
      legend( "topleft",                                     # posicion
              c("Serie real","Ajuste Regresion"),            # texto
              lwd = c(3, 2),                                 # grosor lineas
              col = c('black','red'),                        # color lineas
              bty = "n")                                     # sin caja alrededor de la leyenda
      
      grid()
      
      
      
    }
  })
  
  output$graficoResiduales<-renderPlot({
    serie<-cargarArchivo()
    
    if(!is.null(serie)){
      options(repr.plot.width=10, repr.plot.height=6)
      ## Variable x
      t<- seq(1:length(serie))                          # Variable independiente t: Tiempo
      
      ## Ajuste
      m1 <- lm(serie~t)
      
      ## Extraer residuales modelo lineal
      r1 = m1$residuals
      
      ## Residuales Modelo lineal
      par(mfrow=c(2,2))
      
      plot(t,r1,
           type='l',
           ylab='',main="Residuales Modelo Cuadratico",
           col="red")
      
      abline(h=0,lty=2)       # Linea para la media
      
      plot(density(r1),       # Grafica de densidad
           xlab='x',
           main= 'Densidad Residuales Modelo Cuadratico', 
           col="red")
      
      qqnorm(r1)               # Grafica qqnorm para probar normalidad
      qqline(r1,col=2)         # Linea
      
      acf(r1, ci.type="ma",60) # Prueba ACF
      
      
    }
  })
  
  output$graficoPronostico<-renderPlot({
    
    conjuntos<-conjuntosPrueba()
    serie<-cargarArchivo()
    
    if(!is.null(serie)){
      
      serie.fit<-as.ts(conjuntos$fits)
      serie.for<-as.ts(conjuntos$fors)
      
      options(repr.plot.width=10, repr.plot.height=6)
      ## Variable x
      t<- seq(1:length(serie))                          # Variable independiente t: Tiempo
      
      ## Ajuste
      m1 <- lm(serie~t)
      
      T=length(serie)           # Variable independiente t: Tiempo
      
      t=seq(1:(T-as.numeric(input$periodosPrediccion)))
      
      
      ## Periodos a pronosticar
      m<-as.numeric(input$periodosPrediccion)
      
      ## Variables indicadoras del pronostico
      Itf = seasonaldummyf(serie.for,m)
      
      ## Tiempo 
      tf = seq(T+1,T+m,1)
      
      ## Prediccion
      y1 = predict(m1,data.frame(t = tf,It=I(Itf)))
      
      
      y1<-ts(y1, start = tsp(serie.for)[1],
             #c(as.numeric(input$anioInicio), as.numeric(input$periodoInicio)),         #Año de inicio - periodo de inicio
             frequency = as.numeric(input$periodosPrediccion))
      
      plot(serie,
           type = 'o',
           lwd=2, 
          ylim = c(min(serie)-20,max(serie)+20)) # límites min y max del eje Y
      
      lines(y1,col='red',lwd=2)
      
      legend( "topleft",                                      # posicion
              c("Serie real","Pron. Regresion"),              # texto
              lwd = c(2.5, 2),                                # grosor lineas   
              col = c('black','red'),                         # color lineas
              bty = "n")                                      # sin caja alrededor de la leyenda
      
      grid()
      
    }
  })


  ### Método de tendencia cuadrática
  
  #Gráfico de la serie 
  output$graficoCuadratica<-renderPlot({
    serieC<-cargarArchivo()
    if(!is.null(serieC)){
      options(repr.plot.width=10, repr.plot.height=6)
      plot(serieC)
      
      legend( "top",                                     # posicion
              c("Serie real"),         # texto
              lwd = c(2),                        # grosor lineas
              col = c('black'), # color lineas
              bty = "n")                                     # caja alrededor de la leyenda
    }
  })
  
  #Aplicación de método tendencia cuadrática
  output$metodoCuadratico<-renderPrint({
    conjuntos<-conjuntosPrueba()
    
    if(!is.null(conjuntos$serie)){
      serieC<-conjuntos$serie
      serieC.fit<-as.ts(conjuntos$fits)
      serieC.for<-as.ts(conjuntos$fors)
      
      ## Variable x
      t<-seq(1:length(serieC))                             # Variable independiente t: Tiempo
      tt<-t*t                                         # Parametro t^2
      
      ## Ajuste
      modeloC<-lm(serieC~t+tt)
      
      output$resumenCuadratico<-renderPrint({ summary(modeloC) })
             
      #Dibujar el gráfico de ajuste
      output$graficoAjusteCuadratica<-renderPlot({
        options(repr.plot.width=10, repr.plot.height=6)
        
        plot(t,serieC,                                             # datos de la serie
             type = "o",                                      # o -- overplot
             lwd = 3)                                         # ancho de la linea
        
        
        lines(modeloC$fitted.values, col = "red", lwd = 2)
        
        legend( "topleft",                                     # posicion
                c("Serie real","Ajuste Regresión"),            # texto
                lwd = c(3, 2),                                 # grosor lineas
                col = c('black','red'),                        # color lineas
                bty = "n")                                     # sin caja alrededor de la leyenda
        
        grid()
      })     
      
      #Gráfica residuales
      output$graficoResidualesCuadratica<-renderPlot({
        ## Extraer residuales modelo cuadrático
        r2 = modeloC$residuals
        
        # Residuales Modelo Cuadrático
        par(mfrow=c(2,2))
        options(repr.plot.width=10, repr.plot.height=6)
        
        plot(t,r2,
             type='l',
             ylab='',main="Residuales Modelo Cuadrático",
             col="red")
        
        abline(h=0,lty=2)        # Linea para la media
        
        plot(density(r2),        # Gráfica de densidad
             xlab='x',
             main= 'Densidad Residuales Modelo Cuadrático', 
             col="red")
        
        qqnorm(r2)               # Gráfica qqnorm para probar normalidad
        qqline(r2,col=2)         # Linea
        
        acf(r2, ci.type="ma",60) # Prueba ACF
      })
      
      #Predicción de los n periodos elegidos por el usuario
#       
#       ## Longitud del periodo de ajuste
#       T = length(serieC)
#       
#       ## Variables indicadoras del pronostico
#       Itf = seasonaldummy(serieC.for, as.numeric(input$periodosPrediccion))
#       
#       ## Tiempo 
#       tf = seq(T+1,T+as.numeric(input$periodosPrediccion),1)
#       
#       ## Predicción
#       prediccionC= predict(modeloC,data.frame(t = tf,It=I(Itf)))
#       prediccionC<-ts(prediccionC,freq=frequency(serieC),start=tsp(serieC.for)[1])
#       
#       #Gráfico de la predicción
#       output$graficoPrediccionCuadratica<-renderPlot({
#         options(repr.plot.width=10, repr.plot.height=6)
#         
#         plot(serieC,
#               type = 'o',
#               lwd=2,
#               ylim=c(10.5,15))
#         
#         lines(prediccionC,col='red',lwd=2)
#         
#         legend( "topleft",                                      # posicion
#                 c("Serie real","Pron. Regresión"),              # texto
#                 lwd = c(2.5, 2),                                # grosor lineas   
#                 col = c('black','red'),                         # color lineas
#                 bty = "n")                                      # sin caja alrededor de la leyenda
#         
#         grid()
#       })
      
      #Mostrar la información del método Holt-Winters
      modeloC
    }
  })
    ##FIN tendencia cuadrática

=======
>>>>>>> 635ee8ea1c71fc947eb091bb37f151ff99eb3555
>>>>>>> ced35416a92251b3181b226ae2c22ef7b9e71fde
})