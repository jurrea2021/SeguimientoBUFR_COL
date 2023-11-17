library(shiny)

shinyUI(fluidPage(
  fluidRow(
    column(6,
           selectInput(inputId = 'anio_mes',
                       label = 'PERIODO DE SEGUIMIENTO',
                       choices = c("Octubre de 2023" = "oct-2023",
                                   "Noviembre de 2023" = "nov-2023"
                       )),
           plotlyOutput('graficoCantidades')),
    column(6,
           selectInput(inputId = 'variable',
                       label = 'VARIABLE',
                       choices = c("Precipitacion" = "prec",
                                   "Presion" = "presion",
                                   "Temperatura" = "temp",
                                   "Temperatura Maxima" = "tempMax",
                                   "Temperatura Minima" = "tempMin",
                                   "Direccion del Viento" = "dirV",
                                   "Velocidad del Viento" = "velV",
                                   "Humedad Relativa" = "hum",
                                   "Punto de Rocio" = "pRocio"
                       )),
           #8,
           plotlyOutput('graficoCantidadesVar'))
           ),
  fluidRow(
    column(4,
           leafletOutput("mapBUFR") 
    ),
    column(8,
           plotlyOutput('graficoEstBUFR')
    )
  ),
  fluidRow(
    column(4),
    column(8,
           htmlOutput("frame")
    )
  )
  )
)