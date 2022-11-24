library(shiny) # 1.7.3
library(shinycssloaders) # 1.0.0
library(shinyWidgets) # 0.7.4

source("global.R")

opciones_indicadores = unique(bd$no)
names(opciones_indicadores) <- meta$indicador[-8]
opciones_entidad <- unique(bd$entidad)[-33]

server <- function(input, output, session) {
  #Sugerencia: usar shinywidgets::sliderTextInput
  output$sldAnio <- renderUI({
    ad <- anios_disponibles(input$selIndicador)
    if(length(ad) > 1) {
    sliderTextInput(inputId = "sldAnio",
                label = "Seleccione año:",
                grid = TRUE,
                choices = ad,
                selected = ad[1])
    } else if (length(ad) > 0) {
      selectInput(inputId = "sldAnio",
                  label = "Seleccione Año",
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

  output$grafica_lineas <- renderPlotly({
    gen_lineas(edo_sel = input$selEnt,
               ind_sel = input$selIndicador,
               anio_sel = input$sldAnio)
  })

}

ui <- fluidPage(
  h1("Catálogo de indicadores", style = "color:rgb(0,167,225);"),
  br(),
  sidebarLayout(
    sidebarPanel(
#      h2("Controles"),
      selectInput(inputId = "selIndicador",
                  label = "Seleccione indicador:",
                  choices = opciones_indicadores,
                  selected = opciones_indicadores[1]),
      selectInput(inputId = "selEnt",
                  label = "Seleccione entidad:",
                  choices = opciones_entidad),
      uiOutput("sldAnio")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Barras:",
                 plotOutput("grafica_barras",
                               height = "80vh") %>% withSpinner()),
        tabPanel("Mapa: ",
                 plotOutput("mapa",
                               height = "80vh")  %>% withSpinner()),
        tabPanel("Serie de tiempo:",
                 plotly::plotlyOutput("grafica_lineas", height = "80vh")  %>% withSpinner())
      )
    )
  )
)

shinyApp(ui, server)
