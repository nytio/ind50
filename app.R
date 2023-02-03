options(scipen = 999)
library(shiny) # 1.7.4
library(shinyWidgets) # 0.7.6
library(shinycssloaders) # 1.0.0
library(tidyverse) # 1.3.2
library(openxlsx) # 4.2.5.1
source("global.R")

#@todo TEST probar con diferentes niveles de desagregación geográfica
# Agregar pestañas
# [-] Prospectiva (Si hay más de 10 datos para proyectar)

ui <- fluidPage(
  includeCSS("www/styles.css"),
  titlePanel("Catálogo de indicadores"),
  #br(),
  sidebarLayout(
    sidebarPanel(
      # Controles
      selectInput(inputId = "selColeccion",
                  label = "Categoría:",
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
      #                    choices = list("Ocultar total" = 1, "Resaltar" = 2),
      #                    selected = c(1, 2)),
      hr(),
      helpText("© 2023 Gobierno del Estado de Guanajuato")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(title = "Tabulado",
                 div(style="position: relative;",
                 div(style="position:absolute;left:calc(80%);top:0px;margin-top:6px;z-index:2",downloadButton("downloadData", "Descargar", icon = icon("download", lib = "glyphicon"))),
                 div(style="position:absolute;left:0px;top:0px;width:calc(80%);z-index:1", DT::dataTableOutput('tab1')) ),
                 icon = icon("table")),
        tabPanel(title = "Gráfica",
                 plotOutput("grafica_barras", height = "85vh") %>% withSpinner(type = 4),
                 icon = icon("bar-chart")),
        tabPanel(title = "Mapa",
                 plotOutput("grafica_mapa", height = "85vh") %>% withSpinner(type = 4),
                 icon = icon("map-marker", lib = "glyphicon")),
        tabPanel(title = "Serie",
                 plotOutput("grafica_lineas", height = "85vh") %>% withSpinner(type = 4),
                 icon = icon("stats", lib = "glyphicon")),
        # tabPanel(title = "Prospectiva",
        #           br(), icon = icon("circle-arrow-up", lib = "glyphicon")),
        tabPanel(title = "Metadato",
                 DT::dataTableOutput('tab2'),
                 icon = icon("info-sign", lib = "glyphicon")),
        tabPanel(title = "Ayuda",
                 h4("Descripción"),
                 p("El proyecto Catálogo de Indicadores ofrece la posibilidad de explorar una amplia variedad de datos relevantes para el Plan Estatal de Desarrollo. Al elegir una categoría entre: Población, Fecundidad, Mortalidad, Migración, Vivienda y Socioeconómica, se accede a una lista de indicadores correspondientes a esa categoría. Además, se puede elegir entre una desagregación municipal o entidad federativa, así como también seleccionar el año de la información deseada."),
                 p("El proyecto presenta una visualización completa y detallada de los datos elegidos, incluyendo un tabulado, una gráfica de barras, un mapa y una serie temporal. Todas estas herramientas permiten una comprensión profunda y un análisis más eficiente de los datos. Además, se pueden descargar los tabulados en formato Excel y se pueden guardar las imágenes de las gráficas y mapas desde el navegador. Por último, se proporciona información detallada sobre las fuentes utilizadas en la elaboración de los metadatos, garantizando la transparencia y la integridad de la información."),
                 icon = icon("question-sign", lib = "glyphicon"))
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
  
  # Archivo descargable del conjunto de datos seleccionado ----
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
