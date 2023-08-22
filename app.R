options(scipen = 999)
library(shiny) # 1.7.4
library(shinyWidgets) # 0.7.6
library(shinycssloaders) # 1.0.0
library(tidyverse) # 2.0.0
library(DT) # 0.27
source("global.R")

#@todo TEST probar con diferentes niveles de desagregación geográfica
# Agregar pestañas
# [-] Prospectiva (Si hay más de 10 datos para proyectar)

ui <- fluidPage(
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
  titlePanel("Sistema Estatal de Información Estadística y Geográfica: Catálogo de indicadores"),
  #br(),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        type = "hidden", # pills
        tabPanelBody("Indicadores",
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
      icon = icon("bookmark")),
      tabPanelBody("Proyecto",
       selectInput(inputId = "selTema",
                   label = "Tema:",
                   choices = opciones_panel,
                   selected = opciones_panel[1]),
       icon = icon("database")
      )),
      # hr(),
      #  checkboxGroupInput("checkGroup", label = "Opciones:", 
      #                     choices = list("Ocultar total" = 1, "Resaltar" = 2),
      #                     selected = c(1, 2)),
      hr(),
      helpText("© 2023 Gobierno del Estado de Guanajuato")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(title = "Tabulado",
                 div(class="boxtab0",
                 div(class="boxtab1", downloadButton("downloadData", "Descargar", icon = icon("download", lib = "glyphicon"))),
                 div(class="boxtab2", DTOutput('tab1'))),
                 icon = icon("table")),
        tabPanel(title = "Gráfica",
                 plotOutput("grafica_barras", height = "85vh") |> withSpinner(type = 4),
                 icon = icon("bar-chart")),
        tabPanel(title = "Mapa",
                 plotOutput("grafica_mapa", height = "85vh") |> withSpinner(type = 4),
                 icon = icon("map-marker", lib = "glyphicon")),
        tabPanel(title = "Serie",
                 plotOutput("grafica_lineas", height = "85vh") |> withSpinner(type = 4),
                 icon = icon("stats", lib = "glyphicon")),
        # tabPanel(title = "Prospectiva",
        #           br(), icon = icon("circle-arrow-up", lib = "glyphicon")),
        tabPanel(title = "Metadato",
                 DTOutput('tab2'),
                 # checkboxInput("show_tab", "Mostrar catálogo de indicadores"),
                 # conditionalPanel(condition = "input.show_tab",
                 #                  DTOutput('tab3')),
                 icon = icon("info-sign", lib = "glyphicon")),
        tabPanel(title = "Ayuda",
                 h4("Descripción"),
                 p("El proyecto Catálogo de Indicadores ofrece la posibilidad de explorar una amplia variedad de datos relevantes para el Plan Estatal de Desarrollo 2050. Al elegir una categoría entre: Población, Fecundidad, Mortalidad, Migración, Vivienda y Socioeconómica, se accede a una lista de indicadores correspondientes a esa categoría. Además, se puede elegir entre una desagregación municipal o entidad federativa, así como también seleccionar el año de la información deseada."),
                 p("El proyecto presenta una visualización completa y detallada de los datos elegidos, incluyendo un tabulado, una gráfica de barras, un mapa y una serie temporal. Todas estas herramientas permiten una comprensión profunda y un análisis más eficiente de los datos. Además, se pueden descargar los tabulados en formato Excel y se pueden guardar las imágenes de las gráficas y mapas desde el navegador. Por último, se proporciona información detallada sobre las fuentes utilizadas en la elaboración de los metadatos, garantizando la transparencia y la integridad de la información."),
                 h4("Recursos adicionales"),
                 p("Además de la información accesible en la plataforma, el proyecto Catálogo de Indicadores también ofrece la opción de descargar archivos en formato Excel con proyecciones y pirámides de población a nivel municipal, cubriendo desde 1990 hasta 2050. Estos archivos adicionales brindan una mayor profundidad de análisis a aquellos que buscan explorar los datos con más detalle. Para los interesados en entender cómo se ha implementado el proyecto, también se ofrece el acceso al código fuente, permitiendo mejorar o modificar el proyecto."),
                 tags$ul(tags$li("Descarga de pirámides de población:", a(href = 'datos/piramide.xlsx', '/piramide.xlsx', target='_blank')),
                         tags$li("Descarga de proyecciones de población:", a(href = 'datos/proymun.xlsx', '/proymun.xlsx', target='_blank')),
                         tags$li("Encuesta Nacional sobre Diversidad Sexual y de Género:", a(href = 'https://www.inegi.org.mx/programas/endiseg/2021/', '/endiseg/2021/', target='_blank')),
                         tags$li("Acceso al código fuente:", a(href = 'https://github.com/nytio/ind50', '/ind50', target='_blank'))),
                 icon = icon("question-sign", lib = "glyphicon")),
        selected = "Ayuda"
      )
    )
  )
)

server <- function(input, output, session) {
  anios_disponibles <- reactive({
    bd |>
      dplyr::filter(no == input$selIndicador) |>
      dplyr::filter(ambito == input$selEnt) |>
      pull(year) |>
      unique()
  })
  
  observeEvent(input$selTema, {
    actualiza_coleccion(input$selTema)
    updateSelectInput(
      session = session,
      inputId = "selColeccion",
      choices = opciones_coleccion)
  }, ignoreInit = TRUE)
  
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
                            selected = usel)#,
                            # from_fixed = NULL,
                            # to_fixed = NULL)
    } else if(length(ad) > 0) {
      updateSliderTextInput(session = session,
                            inputId = "selAnio",
                            choices = c("ND", ad),
                            selected = ad)#,
                            # from_fixed = ad)
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
  
  output$tab1 <- renderDT({
    datasetInput()
  })
  
  # Archivo descargable del conjunto de datos seleccionado ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("tbl", input$selIndicador, "_", input$selAnio, "_", input$selEnt, ".xlsx")
    },
    content = function(file) {
      descargar(datasetInput(), input$selAnio, file)
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
  output$tab2 <- renderDT({
    datasetInput2()
  })
  
  datasetInput3 <- reactive({
    tabulado3()
  })
  output$tab3 <- renderDT({
    datasetInput3()
  })
}

shinyApp(ui, server)
