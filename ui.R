
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
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
      tabPanel("Suavizado Browniano")
    )
  )
)#Fin pageWithSideBar
)#Fin shinyUI