options(scipen = 999)
library(shiny) # 1.7.3
library(shinycssloaders) # 1.0.0
library(shinyWidgets) # 0.7.5
library(tidyverse) # 1.3.1
source("global.R")

#todo@ TEST probar con diferentes niveles de desagregación geográfica
# Agregar opciones en panel:
# [*] Ocultar total
# [*] Resaltar Guanajuato
# Agregar pestañas
# [-] Tabulado
# [-] Prospectiva (Si hay más de 10 datos para Guanajuato)
# [-] Metadatos

ui <- fluidPage(
  includeCSS("www/styles.css"),
  titlePanel("Catálogo de indicadores"),
  #br(),
  sidebarLayout(
    sidebarPanel(
      # Controles
      selectInput(inputId = "selColeccion",
                  label = "Colección:",
                  choices = opciones_coleccion,
                  selected = opciones_coleccion[1]),
      selectInput(inputId = "selIndicador",
                  label = "Indicador:",
                  choices = opciones_indicadores,
                  selected = opciones_indicadores[1]),
      radioButtons(inputId = "selEnt",
                  label = "Desagregación:",
                  choices = opciones_entidad),
      uiOutput("sldAnio"),
      # hr(),
      # checkboxGroupInput("checkGroup", label = "Opciones:", 
      #                    choices = list("Ocultar total" = 1, "Resaltar Guanajuato" = 2),
      #                    selected = c(1, 2)),
      hr(),
      helpText("© 2023 Gobierno del Estado de Guanajuato")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(title = "Tabulado",
                 br()),
        tabPanel(title = "Gráfica",
                 plotOutput("grafica_barras", height = "80vh") %>% withSpinner()),
        tabPanel(title = "Mapa",
                 plotOutput("mapa", height = "80vh") %>% withSpinner()),
        tabPanel(title = "Serie",
                 plotOutput("grafica_lineas", height = "80vh") %>% withSpinner()),
        tabPanel(title = "Prospectiva",
                 br()),
        tabPanel(title = "Metadato",
                 br(),
                 icon = icon("book"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$selColeccion, {
    actualiza_indicador(input$selColeccion)
    updateSelectInput(
      session = session,
      inputId = "selIndicador",
      choices = opciones_indicadores,
      selected = NULL
    )
    actualiza_bd(indicadores[1, 1])
    updateRadioButtons(inputId = "selEnt",
                       label = "Desagregación:",
                       choices = opciones_entidad)
  }, ignoreInit = TRUE)
  
  observeEvent(input$selIndicador, {
    actualiza_bd(input$selIndicador)
    updateRadioButtons(inputId = "selEnt",
                       label = "Desagregación:",
                       choices = opciones_entidad)
  }, ignoreInit = TRUE)
  
  #Sugerencia: usar shinywidgets::sliderTextInput
  output$sldAnio <- renderUI({
    ad <- anios_disponibles(input$selIndicador)
    if(length(ad) > 1) {
      sliderTextInput(inputId = "sldAnio",
                      label = "Año:",
                      grid = TRUE,
                      choices = ad,
                      selected = ad[length(ad)])
    } else if (length(ad) > 0) {
      selectInput(inputId = "sldAnio",
                  label = "Año",
                  choices = ad,
                  selected = ad[1],
                  selectize = FALSE)
    } else {
      br()
    }
  })
  
  output$grafica_barras <- renderPlot({
    gen_barras(edo_sel = input$selEnt,
               ind_sel = input$selIndicador,
               anio_sel = input$sldAnio)
  })
  
  output$mapa <- renderPlot({
    gen_mapa(edo_sel = input$selEnt,
             ind_sel = input$selIndicador,
             anio_sel = input$sldAnio)
  })
  
  output$grafica_lineas <- renderPlot({
    gen_lineas(edo_sel = input$selEnt,
               ind_sel = input$selIndicador,
               anio_sel = input$sldAnio)
  })
  
}

shinyApp(ui, server)
