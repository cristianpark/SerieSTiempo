
# Archivo ui.R
# @author   Cristian Gómez Alvarez <cristianpark@gmail.com>
# @contributor    Juan Camilo Cuartas <juancamilo.cuartas@gmail.com>
#
# Archivo con la interfaz gráfica de la aplicación Shiny para series de tiempo
# 
# http://www.rstudio.com/shiny/
#

library(shiny)

shinyUI(pageWithSidebar(
  # Application title
  headerPanel(h2("Predicción en Series de Tiempo", align="center")),
  
  #Sidebar
  sidebarPanel(
    tabsetPanel(
      tabPanel("Serie",
        br(),
        fileInput('archivoSerie', 'Archivo de la serie de tiempo...', accept=c('text/plain')),
        htmlOutput("labelPreviewArchivo"),
        verbatimTextOutput("previewArchivo"),
        selectInput("decimalSep", "Separador de decimales", choices=list(".", ","), multiple = FALSE,
                    selectize = TRUE, width = NULL, size = NULL),
        selectInput("datosSep", "Separador de datos", choices=list("Tab"="\t", "Espacio"=" ", "Espacio doble"="  "), multiple = FALSE,
                    selectize = TRUE, width = NULL, size = NULL),
        selectInput("encabezado", "Encabezado", choices=list("Si"=TRUE, "No"=FALSE), selected = c(TRUE), multiple = FALSE,
                    selectize = TRUE, width = NULL, size = NULL),
        strong("Número de líneas a omitir"),
        textInput("lineasSaltar", width = "50px", label="", value = 0),
        #Periodicidad de la serie
        selectInput("periodicidad", "Periodicidad serie", choices=list("Mensual"=12, "Bimestral"=6, "Trimestral"=4, "Cuatrimestres"=3, "Semestral"=2, "Diaria"=365), multiple = FALSE,
                        selectize = TRUE, width = NULL, size = NULL),
        strong("Año de inicio"),
        textInput("anioInicio", width = "70px", label="", value = 2010),
        strong("Periodo inicio"),
        textInput("periodoInicio", width = "50px", label="", value = 1),
        textInput("columnaDatos", width = "100px", label="Columna datos", value = "x")
      ),
      tabPanel("Predicción", 
         br(),
         strong("Periodos predicción"),
         textInput("periodosPrediccion", width = "50px", label="", value = 4),
         strong("Periodos validación"),
         textInput("periodosValidacion", width = "50px", label="", value = 4)
      )
    )
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    tabsetPanel(
      ##Suavizado exponencial
      tabPanel("Suavizado Exponencial",
        plotOutput("graficoSuavizado", height = "350px"),
        strong("Método Holt-Winters"),
        verbatimTextOutput("holtWinters"),
        br(),
        strong("Error cuadrático"),
        htmlOutput("errorWinters"),
        plotOutput("graficoAjusteSE"),
        strong("Predicción"),
        plotOutput("graficoPrediccionSE"),
        strong("Predicción con Forecast"),
        plotOutput("graficoPrediccionFSE")
      ),      
      ##Suavizado exponencial doble
      tabPanel("Exponencial Doble",
         plotOutput("graficoSuavizadoD", height = "350px"),
         strong("Suavizado Exponencial Doble"),
         verbatimTextOutput("holtWintersD"),
         br(),
         strong("Error cuadrático"),
         htmlOutput("errorWintersD"),
         plotOutput("graficoAjusteSED"),
         strong("Predicción"),
         plotOutput("graficoPrediccionSED"),
         strong("Predicción con Forecast"),
         plotOutput("graficoPrediccionFSED")
      ),

      tabPanel("Pronóstico con Regresión",
               br(),
               strong("Serie"),
               plotOutput("graficoRegresion", height = "350px"),
               strong("Resumen de la regresion"),
               verbatimTextOutput("resumenRegresion"),
               strong("Grafica de ajuste."),
               plotOutput("graficoAjuste", height = "350px"),
               strong("Analisis de residuales"),
               plotOutput("graficoResiduales", height = "350px"),
               strong("Grafico Pronostico"),
               plotOutput("graficoPronostico", height = "350px")
               
      ),
      
      ##Regresión cuadrática
      tabPanel("Tendencia Cuadrática",
               plotOutput("graficoCuadratica", height = "350px"),
               strong("Método de Tendencia Cuadrática"),
               verbatimTextOutput("metodoCuadratico"),
               br(),
               strong("Resumen Regresión"),
               verbatimTextOutput("resumenCuadratico"),
               plotOutput("graficoAjusteCuadratica"),
               strong("Análisis residuales"),
               plotOutput("graficoResidualesCuadratica")
#                strong("Predicción"),
#                plotOutput("graficoPrediccionCuadratica")
      )
    )
  )
)#Fin pageWithSideBar
)#Fin shinyUI