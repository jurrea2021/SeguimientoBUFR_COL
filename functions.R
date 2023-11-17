filtroVariablesBUFR = function(id,labelId) {
  selectInput(id,
              label = labelId,
              choices = c("Precipitacion" = "prec",
                          "Presion" = "presion",
                          "Temperatura" = "temp",
                          "Temperatura Maxima" = "tempMax",
                          "Temperatura Minima" = "tempMin",
                          "Direccion del Viento" = "dirV",
                          "Velocidad del Viento" = "velV",
                          "Humedad Relativa" = "hum",
                          "Punto de Rocio" = "pRocio"
              ))
}