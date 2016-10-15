
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
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
      
      serie.fit<-primerosSerie(serie, as.numeric(input$periodosPrediccion))
      serie.for<-ultimosSerie(serie, as.numeric(input$periodosPrediccion))
    }
    
    #Vector con la serie y los conjuntos de prueba y 
    return(list(serie=serie, fits=serie.fit, fors=serie.for))
  })
  
  ### Método de suavizado exponencial
  
  #Gráfico de la serie 
  output$graficoSuavizado<-renderPlot({
    serie<-cargarArchivo()
    if(!is.null(serie)){
      options(repr.plot.width=10, repr.plot.height=6)
      plot(serie)
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
      cat(file=stderr(), "For ", serie.for, "\n")
      
      modelo<-HoltWinters(x = serie.fit,      # conjunto de entrenamiento de la serie
                        alpha = NULL,    # NULL indica que se calcule el alpha óptimo
                        beta = FALSE,   # no se considera esta componente
                        gamma = FALSE)  # no se considera esta componente
      
#       cat(file=stderr(), "For ", modelo, "\n")        

        output$errorWinters<-renderText({modelo$SSE})
        modelo
    }
  })
})