library(shiny) # 1.7.3
library(shinycssloaders) # 1.0.0
library(shinyWidgets) # 0.7.5
library(tidyverse) # 1.3.1
source("global.R")

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
  
  output$grafica_lineas <- renderPlotly({
    gen_lineas(edo_sel = input$selEnt,
               ind_sel = input$selIndicador,
               anio_sel = input$sldAnio)
  })
  
  observeEvent(input$selColeccion, {
    indicadores <<- dbGetQuery(con, paste0("SELECT idserie, indicador FROM viewb1 WHERE idcoleccion = ", input$selColeccion))
    opciones_indicadores <<- unique(indicadores$idserie)
    names(opciones_indicadores) <<- indicadores$indicador
    updateSelectInput(
      session = session,
      inputId = "selIndicador",
      choices = opciones_indicadores,
      selected = NULL
    )
  }, ignoreInit = TRUE)
  
  observeEvent(input$selIndicador, {
    meta <<-
      dbGetQuery(con, paste0("SELECT * FROM viewb2 WHERE idserie = ", indicadores[1, 1]))
    
    idambito <- unique(meta$idambito)
    if (length(idambito) == 1) {
      if (idambito == 2) {
        opciones_entidad <<- c(2)
        names(opciones_entidad) <<- c("Entidad federativa")
      } else
        if (idambito == 4) {
          opciones_entidad <<- c(1, 2, 5)
          names(opciones_entidad) <<-
            c("Municipio", "Entidad federativa", "Localidad")
        } else
          if (idambito == 3) {
            opciones_entidad <<- c(1, 2)
            names(opciones_entidad) <<- c("Municipio", "Entidad federativa")
          } else
            if (idambito == 8) {
              opciones_entidad <<- c(1, 2, 5, 7)
              names(opciones_entidad) <<-
                c("Municipio",
                  "Entidad federativa",
                  "Localidad",
                  "Estados de EE.UU.")
            } else
              if (idambito == 1) {
                opciones_entidad <<- c(1)
                names(opciones_entidad) <<- c("Municipio")
              } else
                if (idambito == 5) {
                  opciones_entidad <<- c(5)
                  names(opciones_entidad) <<- c("Localidad")
                } else
                  if (idambito == 6) {
                    opciones_entidad <<- c(6)
                    names(opciones_entidad) <<- c("País")
                  } else
                    if (idambito == 7) {
                      opciones_entidad <- c(7)
                      names(opciones_entidad) <- c("Estados de EE.UU.")
                    }
    }
    
    campo1 <-
      dbGetQuery(con, paste0(
        "SELECT * FROM view04 WHERE ",
        paste("idind = ", meta$idind, collapse = " OR ")
      ))
    
    use_sql <- paste0(
      'SELECT geografico.cve, geografico.nom, ',
      paste(campo1$tabla, campo1$mnemonico, sep = "."),
      " AS valor FROM ",
      campo1$tabla,
      " INNER JOIN geografico ON geografico.pais = ",
      campo1$tabla,
      ".pais AND geografico.ent = ",
      campo1$tabla,
      ".ent AND geografico.mun = ",
      campo1$tabla,
      ".mun AND geografico.loc = ",
      campo1$tabla,
      ".loc"
    )
    
    bd <<- NULL
    for (i in 1:length(use_sql)) {
      cs <-
        cbind(meta$idserie[i], cbind(meta$fecha[i], dbGetQuery(con, use_sql[i])))
      bd <<- rbind(bd, cs)
    }
    colnames(bd)[1] = "no"
    colnames(bd)[2] = "year"
    
    updateSelectInput(inputId = "selEnt",
                label = "Seleccione nivel de desagregación:",
                choices = opciones_entidad)
  }, ignoreInit = TRUE)
  
}

ui <- fluidPage(
  h1("Catálogo de indicadores", style = "color:rgb(0,167,225);"),
  br(),
  sidebarLayout(
    sidebarPanel(
      #      h2("Controles"),
      selectInput(inputId = "selColeccion",
                  label = "Seleccione colección:",
                  choices = opciones_coleccion,
                  selected = opciones_coleccion[1]),
      selectInput(inputId = "selIndicador",
                  label = "Seleccione indicador:",
                  choices = opciones_indicadores,
                  selected = opciones_indicadores[1]),
      selectInput(inputId = "selEnt",
                  label = "Seleccione nivel de desagregación:",
                  choices = opciones_entidad),
      uiOutput("sldAnio")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Gráfica",
                 plotOutput("grafica_barras",
                            height = "80vh") |> withSpinner()),
        tabPanel("Mapa",
                 plotOutput("mapa",
                            height = "80vh")  |> withSpinner()),
        tabPanel("Serie",
                 plotly::plotlyOutput("grafica_lineas", height = "80vh")  |> withSpinner())
      )
    )
  )
)

shinyApp(ui, server)
