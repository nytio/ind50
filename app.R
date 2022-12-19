options(scipen = 999)
library(shiny) # 1.7.3
library(shinycssloaders) # 1.0.0
library(shinyWidgets) # 0.7.5
library(tidyverse) # 1.3.1
source("global.R")

#todo@ TEST probar con diferentes niveles de desagregación geográfica

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

ui <- fluidPage(
  h1("Catálogo de indicadores", style = "color:rgb(0,167,225);"),
  #br(),
  sidebarLayout(
    sidebarPanel(
      #      h2("Controles"),
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
      br(),
      helpText("© 2023 Gobierno del Estado de Guanajuato")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Gráfica",
                 plotOutput("grafica_barras", height = "80vh") %>% withSpinner()),
        tabPanel("Mapa",
                 plotOutput("mapa", height = "80vh") %>% withSpinner()),
        tabPanel("Serie",
                 plotOutput("grafica_lineas", height = "80vh") %>% withSpinner())
      )
    )
  )
)

shinyApp(ui, server)
