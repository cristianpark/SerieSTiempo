
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)

shinyUI(pageWithSidebar(
  # Application title
  headerPanel("Predicción en Series"),
  
  #Sidebar
  sidebarPanel(
    tabsetPanel(
      tabPanel("Serie",
        br(),
        fileInput('archivoSerie', 'Archivo de la serie de tiempo...', accept=c('text/plain')),
        selectInput("decimalSep", "Separador de decimales", choices=list(".", ","), multiple = FALSE,
                    selectize = TRUE, width = NULL, size = NULL),
        selectInput("datosSep", "Separador de datos", choices=list("Tab"="\t", "Espacio"=" ", "Espacio doble"="  "), multiple = FALSE,
                    selectize = TRUE, width = NULL, size = NULL),
        selectInput("encabezado", "Encabezado", choices=list("Si"=TRUE, "No"=FALSE), selected = c(TRUE), multiple = FALSE,
                    selectize = TRUE, width = NULL, size = NULL),
        conditionalPanel(
          condition = "input.pruebaBoton",
          strong("Número de líneas a omitir"),
          textInput("lineasSaltar", width = "50px", label="", value = 0)
        ),
        strong("Año de inicio"),
        textInput("añoInicio", width = "70px", label="", value = 2010),
        strong("Periodo inicio"),
        textInput("periodoInicio", width = "50px", label="", value = 1),
        textInput("columnaDatos", width = "100px", label="Columna datos", value = "x")
      ),
      tabPanel("Predicción", 
         strong("Periodos predicción"),
         textInput("periodosPrediccion", width = "50px", label="", value = 4)
      )
    )
    #Periodicidad de la serie
#     selectInput("periodicidad", "Periodicidad serie", choices=list("Mensual"=12, "Bimestral"=6, "Trimestral"=4, "Cuatrimestres"=3, "Semestral"=2, "Diaria"=365), multiple = FALSE,
#                 selectize = TRUE, width = NULL, size = NULL),
#     
#     #Fechas de la serie
#     dateRangeInput("fechasSerie", "Fechas:",
#                    start  = "2001-01-01",
#                    end    = "2010-12-31",
#                    min    = "1990-01-01",
#                    max    = "2016-06-30",
#                    format = "yyyy-mm-dd",
#                    separator = " - "),
    
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    h4("RESULTADOS", align="center"),
    tags$head(tags$script(src = "message-handler.js")),    
    textOutput("salida"),
    plotOutput("distPlot"),
    h4("Siguiente parámetro"),
    textOutput("normalidad")
  )
))
