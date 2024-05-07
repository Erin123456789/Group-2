library(shiny)
library(RMariaDB)
library(DBI)

# Connect to Marian DB
db <- dbConnect(RMariaDB::MariaDB(), 
                user='root', 
                password='MarianHighSchool402$)@', 
                dbname='marian', host='34.135.251.192')

# Retrieve database column names for different tests
db_columns <- list(
  ACT = dbGetQuery(db, "DESCRIBE marian.ACT")$Field,
  PreACT = dbGetQuery(db, "DESCRIBE marian.PreACT")$Field,
  PreACT89 = dbGetQuery(db, "DESCRIBE marian.PreACT89")$Field,
  PreSAT = dbGetQuery(db, "DESCRIBE marian.PreSAT")$Field
)

# Disconnect from Marian DB until we upload
dbDisconnect(db)

# Define UI
ui <- fluidPage(
  titlePanel("Standardized Testing Data"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", label = "Select CSV file", 
                accept = ".csv"),
      radioButtons("radio", "Select Exam",
                   choices = list("ACT Data" = "ACT", "PreACT Data" = "PreACT", "PreACT 8/9 Data" = "PreACT89", "PreSAT Data" = "PreSAT"),
                   selected = "ACT"),
      uiOutput("column_match_ui"),
      uiOutput("update_button"),
    ),
    
    mainPanel(
      uiOutput("preview_header"),
      div(style = "overflow-y: auto; 
          overflow-x: auto; 
          max-height: 300px; 
          width: auto; 
          display: inline-block;",
          
          tableOutput("contents"),
      ),
      
    )
  )
)

server <- function(input, output) {
  
  uploaded_data <- reactiveVal()
  
  observeEvent(input$file, {
    req(input$file)
    df <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
    uploaded_data(df)  # Store uploaded data
  })
  
  output$update_button <- renderUI({
    if (!is.null(uploaded_data()) && nrow(uploaded_data()) > 0) {
      actionButton("upload", label = "Upload to Database")
    }
  })
  
  output$preview_header <- renderUI({
    if (!is.null(uploaded_data())) {
      h4(input$file$name)
    }
  })
  
  output$contents <- renderTable({
    req(uploaded_data())
    uploaded_data()
  })
  
  output$column_match_ui <- renderUI({
    req(uploaded_data(), input$radio)
    df_cols <- names(uploaded_data())
    db_cols <- db_columns[[input$radio]]
    
    do.call(tagList, lapply(db_cols, function(col) {
      fluidRow(
        column(4, strong(col)),
        column(8, selectInput(paste0("match_", col), NULL, 
                  choices = c("-----", df_cols), 
                  selected = df_cols[df_cols == col]))
      )
    }))
  })
  
  observeEvent(input$upload, {
    req(uploaded_data(), input$radio)
    data_to_upload <- uploaded_data()
    
    # Map CSV columns to database columns
    mappings <- sapply(db_columns[[input$radio]], function(col) input[[paste0("match_", col)]])
    
    # Check if any field is still "-----"
    if ("-----" %in% mappings) {
      showNotification("Please make sure all columns are selected.", type = "error", duration = 5)
      return()
    }
    
    data_to_upload <- data_to_upload[, mappings, drop = FALSE]
    names(data_to_upload) <- db_columns[[input$radio]]
    
    # Connect to database
    db <- dbConnect(RMariaDB::MariaDB(), 
                    user='root', 
                    password='MarianHighSchool402$)@', 
                    dbname='marian', host='34.135.251.192')

    tryCatch({
      dbBegin(db)
      dbWriteTable(db, input$radio, data_to_upload, append = TRUE, overwrite = FALSE)
      dbCommit(db)
      showNotification("Data uploaded successfully!", type = "message", duration = 5)
    }, 
    
    # Error handling
    error = function(e) {
      dbRollback(db)
      showNotification(sprintf("Failed to upload data: %s", e$message), type = "error", duration = 5)
    })
    dbDisconnect(db)
    
  })
}

shinyApp(ui, server)
