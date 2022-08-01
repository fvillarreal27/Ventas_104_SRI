# ==============================================================================
# Informacion de ventas locales SRI (Estadisticas Multidimensionales)
# DNPRMF
# Fabian Villarreal
#
# SRI Formulario 104 por CIIU
# https://srienlinea.sri.gob.ec/saiku-ui/
# ==============================================================================

# Librerias
pacman::p_load(tidyverse, readxl, openxlsx, dplyr, ggplot2, ggfortify, foreign, 
               timeDate, forecast, xts, urca, tseries, lubridate, stringi, 
               stringr, reshape2, expsmooth, seasonal, Metrics, highcharter)

#library(shiny)

# Colores
my_pal <- c('#1E4976', '#5FA3BF', '#BBBE64', '#EFCB68', '#E5825E', '#9D3C11')

# Directorio
setwd('D:/fvillarreal/Programacion Sector Real/Otros indicadores/PruebaSRICIIU')


# Datos
load('Resultados/lst_df_charts.RData')

labels_ciiu$cod_ciiu_shiny <- paste('CIIU', labels_ciiu$cod_ciiu)



# Shiny App 

# Definir User Interface
ui <- fluidPage(
  
  # Descripcion
  p(HTML(
    '</br><b><span style="font-size: 18px">
    Ventas locales SRI: Formulario 104</span></b></br>
    <span style="font-size: 14px">
    <b>Banco Central del Ecuador: DNPRMF</span></b></b>')),
  
  fluidRow(
    column(width = 12,
           HTML(
             'Ventas locales por CIIU Rev.4 del Formulario 104 del SRI. Las ventas locales incluyen:</br>
             <ul>
             <li>Ventas locales gravadas con tarifa 12%</li>
             <li>Ventas locales gravadas con tarifa 0% (sin credito tributario)</li>
             <li>Ventas locales gravadas con tarifa 0% (con credito tributario)</li>')),
    column(width = 12, HTML('</br>'))),
  
  # Panel de selecciones
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = 'CIIU',
                  label = 'Industria CIIU',
                  choices = labels_ciiu$cod_ciiu,
                  selected = 'CIIU A'),
      radioButtons(input = 'Serie',
                   label = 'Serie',
                   choices = c('Nivel USD', 'Variacion interanual t/t-12'),
                   selected = 'Nivel USD')),
    mainPanel(
      highchartOutput('plot'))))


# Definir server
# server <- function(input, output) {
#   output$plot <- renderPlot({
#     plot(5*2)
# })
# }

server <- function(input, output) {
  output$plot <- renderHighchart({
    if (input$Serie == 'Nivel USD') {
      hchart_level_ciiu[[input$CIIU]]
    } else {
      hchart_vartt12_ciiu[[input$CIIU]]
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
