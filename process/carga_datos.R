library(shiny) # 1.7.4
library(readxl) # 1.4.2
library(DBI) # 1.1.3

ui <- fluidPage(
  titlePanel("Cargar archivo Excel"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Selecciona un archivo de Excel"),
      uiOutput("sheet_selector"),
      textInput("table_name", "Nombre de la nueva tabla", ""),
      actionButton("load", "Crear tabla")
    ),
    mainPanel(
      tableOutput("table")
    )
  )
)

server <- function(input, output, session) {
  # Crear el cuadro combinado para seleccionar la hoja del archivo de Excel
  output$sheet_selector <- renderUI({
    req(input$file)
    sheets <- excel_sheets(input$file$datapath)
    selectInput("sheet", "Selecciona una hoja", choices = sheets)
  })
  
  observeEvent(input$load, {
    req(input$file, input$sheet, input$table_name)
    
    # Verificar si el usuario ingresó un nombre de tabla
    if (input$table_name == "") {
      showModal(modalDialog(
        title = "Error",
        "Por favor, ingrese un nombre para la nueva tabla.",
        easyClose = TRUE
      ))
      return()
    }
    
    # Leer la hoja seleccionada del archivo de Excel
    data <- read_excel(input$file$datapath, sheet = input$sheet)
    
    # Conectar a la base de datos
    con <- dbConnect(odbc::odbc(), "indicadores", timeout = 10) # circinus

    # Crear la nueva tabla con el nombre ingresado por el usuario
    table_name <- input$table_name
    columns <- colnames(data)
    columns_sql <- paste(columns, "VARCHAR(255)", collapse = ", ")
    query_create <- paste0("CREATE TABLE ", table_name, " (", columns_sql, ")")
    dbExecute(con, query_create)

    # Insertar los datos en la nueva tabla
    dbWriteTable(con, table_name, data, append = TRUE, row.names = FALSE)

    # Mostrar la nueva tabla en la aplicación
    output$table <- renderTable({
      dbReadTable(con, table_name)
    })

    # Desconectar de la base de datos
    dbDisconnect(con)
  })
}

shinyApp(ui, server)
