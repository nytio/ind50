options(scipen = 999)
library(shiny) # 1.7.4
library(shinycssloaders) # 1.0.0
library(shinyWidgets) # 0.7.6
library(tidyverse) # 1.3.2
library(openxlsx) # 4.2.5.1
source("global.R")

#todo@ TEST probar con diferentes niveles de desagregación geográfica
# Agregar pestañas
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
      uiOutput("selAnio"),
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
                 DT::dataTableOutput('tab1'),
                 downloadButton("downloadData", "Descargar"),
                 icon = icon("table")),
        tabPanel(title = "Gráfica",
                 plotOutput("grafica_barras", height = "85vh") %>% withSpinner(type = 4),
                 icon = icon("bar-chart")),
        tabPanel(title = "Mapa",
                 plotOutput("grafica_mapa", height = "85vh") %>% withSpinner(type = 4),
                 icon = icon("map-marker")),
        tabPanel(title = "Serie",
                 plotOutput("grafica_lineas", height = "85vh") %>% withSpinner(type = 4)),
        # tabPanel(title = "Prospectiva",
        #          br()),
        tabPanel(title = "Metadato",
                 DT::dataTableOutput('tab2'))
      )
    )
  )
)

server <- function(input, output, session) {
  anios_disponibles <- reactive({
    bd %>%
      filter(no == input$selIndicador) %>%
      filter(ambito == input$selEnt) %>%
      pull(year) %>%
      unique()
  })
  
  output$selAnio <- renderUI({
    ad <- anios_disponibles()
    if(length(ad) > 1) {
      sliderTextInput(inputId = "selAnio",
                      label = "Año:",
                      grid = TRUE,
                      choices = ad,
                      selected = ad[length(ad)])
    } else if (length(ad) > 0) {
      selectInput(inputId = "selAnio",
                  label = "Año",
                  choices = ad,
                  selected = ad[1],
                  selectize = FALSE)
    } else {
      br()
    }
  })
  
  observeEvent(input$selColeccion, {
    actualiza_indicador(input$selColeccion)
    updateSelectInput(
      session = session,
      inputId = "selIndicador",
      choices = opciones_indicadores
    )
  }, ignoreInit = TRUE)
  
  observeEvent(input$selIndicador, {
    actualiza_bd(input$selIndicador)
    ad <- anios_disponibles()
    if(length(ad) > 1) {
      sliderTextInput(inputId = "selAnio",
                      label = "Año:",
                      grid = TRUE,
                      choices = ad,
                      selected = ad[length(ad)])
    } else if (length(ad) > 0) {
      selectInput(inputId = "selAnio",
                  label = "Año",
                  choices = ad,
                  selected = ad[1],
                  selectize = FALSE)
    } else {
      br()
    }
    
    actualiza_opciones_entidad(input$selIndicador, input$selAnio)
    updateRadioButtons(inputId = "selEnt",
                       label = "Desagregación:",
                       choices = opciones_entidad)
  }, ignoreInit = TRUE)
  
  observeEvent(input$selEnt, {
    ad <- anios_disponibles()
    if(length(ad) > 1) {
      sliderTextInput(inputId = "selAnio",
                      label = "Año:",
                      grid = TRUE,
                      choices = ad,
                      selected = ad[length(ad)])
    } else if (length(ad) > 0) {
      selectInput(inputId = "selAnio",
                  label = "Año",
                  choices = ad,
                  selected = ad[1],
                  selectize = FALSE)
    } else {
      br()
    }
  }, ignoreInit = TRUE)
  
  datasetInput <- reactive({
    tabulado(edo_sel = input$selEnt,
             ind_sel = input$selIndicador,
             anio_sel = input$selAnio)
  })
  
  output$tab1 <- DT::renderDataTable({
    datasetInput()
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("tbl", input$selIndicador, "_", input$selAnio, "_", input$selEnt, ".xlsx")
    },
    content = function(file) {
      # Crea un nuevo libro de trabajo
      wb <- createWorkbook()
      
      # Agrega una hoja de trabajo al libro y escribe el dataframe en ella
      addWorksheet(wb, "Hoja1")
      mis_datos <- datasetInput()
      writeData(wb, "Hoja1", substr(mis_datos$x$caption, 10, nchar(mis_datos$x$caption)-10) )
      mis_datos <- mis_datos$x$data
      writeData(wb, "Hoja1", mis_datos, startRow = 3)
      
      # Crea un estilos para la tabla
      titleStyle <- createStyle(
        fontName = "Arial",
        fontSize = 13,
        textDecoration = "bold"
      )
      
      headerStyle <- createStyle(
        fontName = "Arial",
        fontSize = 11,
        fontColour = "white",
        fgFill = "#3465a4",
        halign = "center",
        textDecoration = "bold"
      )
      
      dataStyle <- createStyle(
        fontName = "Arial",
        fontSize = 11,
        borderStyle = "thin"
      )
      
      # Aplica el estilo a las celdas de la tabla
      addStyle(wb, sheet = "Hoja1", style = titleStyle, rows = 1, cols = 1)
      addStyle(wb, sheet = "Hoja1", style = headerStyle, rows = 3, cols = 1:(dim(mis_datos)[2]))
      addStyle(wb, sheet = "Hoja1", style = dataStyle, rows = 4:(dim(mis_datos)[1]+3), cols = 1:(dim(mis_datos)[2]), gridExpand = TRUE)
      
      # Guarda el libro de trabajo en un archivo xlsx
      saveWorkbook(wb, file)
    }
  )
    
  output$grafica_barras <- renderPlot({
    gen_barras(edo_sel = input$selEnt,
               ind_sel = input$selIndicador,
               anio_sel = input$selAnio)
  })
  
  output$grafica_mapa <- renderPlot({
    gen_mapa(edo_sel = input$selEnt,
             ind_sel = input$selIndicador,
             anio_sel = input$selAnio)
  })
  
  output$grafica_lineas <- renderPlot({
    gen_lineas(edo_sel = input$selEnt,
               ind_sel = input$selIndicador)
  })
  
  datasetInput2 <- reactive({
    tabulado2(ind_sel = input$selIndicador)
  })
  output$tab2 <- DT::renderDataTable({
    datasetInput2()
  })
}

shinyApp(ui, server)
