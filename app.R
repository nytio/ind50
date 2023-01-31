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
      sliderTextInput(inputId = "selAnio",
                  label = "Año",
                  choices = c("1990", "1995", "2000", "2005", "2010", "2015", "2020"),
                  selected = "2020",
                  grid = TRUE),
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
    if(length(ad) > 0) {
      if(is.null(input$selAnio)) {
        usel <- ad[length(ad)]
      } else {
        if (input$selAnio %in% ad) {
          usel <- input$selAnio
        } else {
          usel <- ad[length(ad)]
        }
      }
      updateSliderTextInput(session = session,
                        inputId = "selAnio",
                        choices = ad,
                        selected = usel)
    }
    
    actualiza_opciones_entidad(input$selIndicador, input$selAnio)
    if (input$selEnt %in% opciones_entidad) {
      usel <- input$selEnt
    } else {
      usel <- opciones_entidad[length(opciones_entidad)]
    }
    updateRadioButtons(inputId = "selEnt",
                       label = "Desagregación:",
                       choices = opciones_entidad,
                       selected = usel)
  }, ignoreInit = FALSE)
  
  observeEvent(input$selEnt, {
    ad <- anios_disponibles()
    if(length(ad) > 0) {
      if(is.null(input$selAnio)) {
        usel <- ad[length(ad)]
      } else {
        if (input$selAnio %in% ad) {
          usel <- input$selAnio
        } else {
          usel <- ad[length(ad)]
        }
      }
      updateSliderTextInput(session = session,
                            inputId = "selAnio",
                            choices = ad,
                            selected = usel)
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
      metadatos_sel <- meta %>% 
        filter(fecha == input$selAnio) %>%
        select(indicador, descripcion, unidad, fecha,  fuente, producto)
      writeData(wb, "Hoja1", paste("Fuente:", metadatos_sel$fuente), startRow = dim(mis_datos)[1]+5)
      writeData(wb, "Hoja1", paste(metadatos_sel$producto), startRow = dim(mis_datos)[1]+6)
      
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
      
      fuenteStyle <- createStyle(
        fontSize = 9,
      )
      
      # Aplica el estilo a las celdas de la tabla
      addStyle(wb, sheet = "Hoja1", style = titleStyle, rows = 1, cols = 1)
      addStyle(wb, sheet = "Hoja1", style = headerStyle, rows = 3, cols = 1:(dim(mis_datos)[2]))
      addStyle(wb, sheet = "Hoja1", style = dataStyle, rows = 4:(dim(mis_datos)[1]+6), cols = 1:(dim(mis_datos)[2]), gridExpand = TRUE)
      addStyle(wb, sheet = "Hoja1", style = fuenteStyle, rows = (dim(mis_datos)[1]+5):(dim(mis_datos)[1]+7), cols = 1, gridExpand = TRUE, stack = TRUE)
      setColWidths(wb, sheet = "Hoja1", cols = 2:(dim(mis_datos)[2]), widths = "auto")
      
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
