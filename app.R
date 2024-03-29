options(scipen = 999)
library(shiny) # 1.8.0
library(shinyWidgets) # 0.8.0
library(shinycssloaders) # 1.0.0
library(tidyverse) # 2.0.0
library(DT) # 0.31
source("global.R")

#@todo TEST probar con diferentes niveles de desagregación geográfica
# Agregar pestañas
# [-] Prospectiva (Si hay más de 10 datos para proyectar)

ui <- fluidPage(
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
  titlePanel("Sistema Estatal de Información Estadística y Geográfica: Sistema de indicadores"),
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
                 fluidRow(column(4, offset = 8, class = "selInd3",
                                 downloadButton("grafica_barras_svg", "SVG")
                 )),
                 icon = icon("bar-chart")),
        tabPanel(title = "Dispersión",
                 plotOutput("grafica_dispesion", height = "85vh") |> withSpinner(type = 4),
                 fluidRow(
                   column(6, class = "selInd",
                          pickerInput("selIndicadorVis", "Seleccione:",
                                         choices = opciones_indicadores,
                                         selected = opciones_indicadores[1])),
                   column(4, class = "selInd2",
                          materialSwitch("logScaleInput", "Escala logarítmica", value = FALSE, right = TRUE, status = "primary"),
                          materialSwitch("addRegressionInput", "Línea de tendencia", value = FALSE, right = TRUE, status = "primary"),
                          materialSwitch("selSubconjunto", "Filtro específico", value = FALSE, right = TRUE, status = "primary")),
                   column(2, class = "selInd3",
                          downloadButton("grafica_dispesion_svg", "SVG"))
                 ),
                 icon = icon("bar-chart")
        ),
        tabPanel(title = "Mapa",
                 plotOutput("grafica_mapa", height = "85vh") |> withSpinner(type = 4),
                 fluidRow(column(4, offset = 8, class = "selInd3",
                                 downloadButton("grafica_mapa_svg", "SVG")
                 )),
                 icon = icon("map-marker", lib = "glyphicon")),
        tabPanel(title = "Serie",
                 plotOutput("grafica_lineas", height = "85vh") |> withSpinner(type = 4),
                 fluidRow(
                   column(4, class = "selInd",
                          pickerInput("selDesGeo", "Seleccione:", choices = NULL)),
                   column(4, class = "selInd2",
                          materialSwitch("selTotal", "Incluye total", value = TRUE, right = TRUE, status = "primary")),
                   column(4, class = "selInd3",
                          downloadButton("downloadSerie", "Descargar serie",
                                         icon = icon("download", lib = "glyphicon")),
                          downloadButton("grafica_lineas_svg", "SVG"))
                 ),
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
                 p("El proyecto presenta una visualización completa y detallada de los datos elegidos, incluyendo un tabulado, una gráfica de barras, una gráfica de disperción, un mapa y una serie temporal. Todas estas herramientas permiten una comprensión profunda y un análisis más eficiente de los datos. Además, se pueden descargar los tabulados en formato Excel y se pueden guardar las imágenes de las gráficas y mapas desde el navegador. Por último, se proporciona información detallada sobre las fuentes utilizadas en la elaboración de los metadatos, garantizando la transparencia y la integridad de la información."),
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
    updatePickerInput(
      session = session,
      inputId = "selIndicadorVis",
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
      
      if(input$selEnt == "2")
        updatePickerInput(session = session,
                        inputId = "selDesGeo",
                        choices = c("Estado de Guanajuato" = "11",
                                    "Estados Unidos Mexicanos" = "MEX"),
                        selected = "11")
      else
        updatePickerInput(session = session,
                          inputId = "selDesGeo",
                          choices = c("Acámbaro"  = "11002",
                                      "San Miguel de Allende"  = "11003",
                                      "Apaseo el Alto"  = "11004",
                                      "Apaseo el Grande"  = "11005",
                                      "Atarjea"  = "11006",
                                      "Celaya"  = "11007",
                                      "Manuel Doblado"  = "11008",
                                      "Comonfort"  = "11009",
                                      "Coroneo"  = "11010",
                                      "Cortazar"  = "11011",
                                      "Cuerámaro"  = "11012",
                                      "Doctor Mora"  = "11013",
                                      "Dolores Hidalgo"  = "11014",
                                      "Guanajuato"  = "11015",
                                      "Huanímaro"  = "11016",
                                      "Irapuato"  = "11017",
                                      "Jaral del Progreso"  = "11018",
                                      "Jerécuaro"  = "11019",
                                      "León"  = "11020",
                                      "Moroleón"  = "11021",
                                      "Ocampo"  = "11022",
                                      "Pénjamo"  = "11023",
                                      "Pueblo Nuevo"  = "11024",
                                      "Purísima del Rincón"  = "11025",
                                      "Romita"  = "11026",
                                      "Salamanca"  = "11027",
                                      "Salvatierra"  = "11028",
                                      "San Diego de la Unión"  = "11029",
                                      "San Felipe"  = "11030",
                                      "San Francisco del Rincón"  = "11031",
                                      "San José Iturbide"  = "11032",
                                      "San Luis de la Paz"  = "11033",
                                      "Santa Catarina"  = "11034",
                                      "Santa Cruz de Juventino Rosas"  = "11035",
                                      "Santiago Maravatío"  = "11036",
                                      "Silao de la Victoria"  = "11037",
                                      "Tarandacuao"  = "11038",
                                      "Tarimoro"  = "11039",
                                      "Tierra Blanca"  = "11040",
                                      "Uriangato"  = "11041",
                                      "Valle de Santiago"  = "11042",
                                      "Victoria"  = "11043",
                                      "Villagrán"  = "11044",
                                      "Xichú"  = "11045",
                                      "Yuriria"  = "11046"),
                          selected = "11015")
    }
  }, ignoreInit = FALSE)
  
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
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )

  output$downloadSerie <- downloadHandler(
    filename = function() {
      paste0("hist", input$selIndicador, "_", input$selEnt, ".xlsx")
    },
    content = function(file) {
      descargarSerie(edo_sel = input$selEnt, ind_sel = input$selIndicador, file)
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
  
  output$grafica_barras <- renderPlot({
    gen_barras(edo_sel = input$selEnt,
               ind_sel = input$selIndicador,
               anio_sel = input$selAnio)
  })

  output$grafica_barras_svg <- downloadHandler(
    filename = function() {
      paste0("barras", input$selIndicador, "_", input$selEnt, "_", input$selAnio, ".svg")
    },
    content = function(file) {
      grafico <- gen_barras(edo_sel = input$selEnt,
                            ind_sel = input$selIndicador,
                            anio_sel = input$selAnio, titula = FALSE)
      ggsave(file, plot = grafico, device = "svg", width = 14.60, height = 8.15,  units = "in")
    },
    contentType = "image/svg+xml"
  )

  output$grafica_dispesion <- renderPlot({
    gen_dispesion(edo_sel = input$selEnt,
               ind_sel = input$selIndicador,
               anio_sel = input$selAnio,
               ind_selvis = input$selIndicadorVis,
               log_scale = input$logScaleInput,
               add_regression = input$addRegressionInput,
               sel_entidades = input$selSubconjunto)
  })

  output$grafica_dispesion_svg <- downloadHandler(
    filename = function() {
      paste0("dispersion", input$selIndicador, "_", input$selIndicadorVis, "_", input$selAnio, ".svg")
    },
    content = function(file) {
      grafico <- gen_dispesion(edo_sel = input$selEnt,
                               ind_sel = input$selIndicador,
                               anio_sel = input$selAnio,
                               ind_selvis = input$selIndicadorVis,
                               log_scale = input$logScaleInput,
                               add_regression = input$addRegressionInput,
                               sel_entidades = input$selSubconjunto, titula = FALSE)
      ggsave(file, plot = grafico, device = "svg", width = 14.60, height = 8.15,  units = "in")
    },
    contentType = "image/svg+xml"
  )

  output$grafica_mapa <- renderPlot({
    gen_mapa(edo_sel = input$selEnt,
             ind_sel = input$selIndicador,
             anio_sel = input$selAnio)
  })

  output$grafica_mapa_svg <- downloadHandler(
    filename = function() {
      paste0("mapa", input$selIndicador, "_", input$selEnt, "_", input$selAnio,".svg")
    },
    content = function(file) {
      grafico <- gen_mapa(edo_sel = input$selEnt,
                          ind_sel = input$selIndicador,
                          anio_sel = input$selAnio, titula = FALSE)
      ggsave(file, plot = grafico, device = "svg", width = 10.37, height = 7.78,  units = "in")
    },
    contentType = "image/svg+xml"
  )
  
  output$grafica_lineas <- renderPlot({
    gen_lineas(edo_sel = input$selEnt,
               ind_sel = input$selIndicador,
               geo_sel = input$selDesGeo,
               tot_sel = input$selTotal)
  })

  output$grafica_lineas_svg <- downloadHandler(
    filename = function() {
      paste0("serie", input$selIndicador, "_", input$selEnt,".svg")
    },
    content = function(file) {
      grafico <- gen_lineas(edo_sel = input$selEnt,
                            ind_sel = input$selIndicador,
                            titula = FALSE,
                            geo_sel = input$selDesGeo,
                            tot_sel = input$selTotal )
      ggsave(file, plot = grafico, device = "svg", width = 14.60, height = 8.15,  units = "in")
    },
    contentType = "image/svg+xml"
  )

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
