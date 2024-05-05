library(RMariaDB)
library(DBI)
library(shiny)
library(bslib)

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

# UI Definition
ui <- fluidPage(
  titlePanel("Standardized Testing Data"),
  sidebarLayout(
    sidebarPanel(
      card(
        card_header("Select CSV File"),
        fileInput("file", label = NULL),
      ),
      div(style = "height: 20px; background-color: white;"),
      card(
        radioButtons("radio", "Select Option",
                     choices = list("ACT Data" = "ACT", "PreACT Data" = "PreACT", "PreACT 8/9 Data" = "PreACT89", "PreSAT Data" = "PreSAT"),
                     selected = "ACT"),
        actionButton("upload", label="Update")
      )
    ),
    mainPanel(
      tableOutput("contents"),
      uiOutput("column_match_ui"),  # Dynamic UI for column matching
      textOutput("upload_status")
    )
  )
)

server <- function(input, output, session) {
  # Reactive value to store uploaded data
  uploaded_data <- reactiveVal()
  
  # Update uploaded data
  observeEvent(input$file, {
    req(input$file)
    df <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
    uploaded_data(df)  # Store uploaded data
  })
  
  # Render data preview table
  output$contents <- renderTable({
    req(uploaded_data())
    head(uploaded_data(), 10)
  })
  
  # Dynamic UI for column matching
  output$column_match_ui <- renderUI({
    req(uploaded_data(), input$radio)
    df_cols <- names(uploaded_data())
    db_cols <- db_columns[[input$radio]]
    
    # Generate UI elements for each database column
    do.call(tagList, lapply(db_cols, function(col) {
      fluidRow(
        column(4, strong(col)),
        column(8, selectInput(paste0("match_", col), NULL, choices = df_cols, selected = df_cols[df_cols == col]))
      )
    }))
  })
  
  # Handle file upload and insert data into database
  observeEvent(input$upload, {
    req(uploaded_data())
    # Map the columns based on the selected matches
    data_to_upload <- uploaded_data()
    mappings <- sapply(db_columns[[input$radio]], function(col) input[[paste0("match_", col)]])
    data_to_upload <- data_to_upload[, mappings, drop = FALSE]
    names(data_to_upload) <- db_columns[[input$radio]]
    
    tryCatch({
      # Connect back to the Database for final upload
      db <- dbConnect(RMariaDB::MariaDB(), 
                      user='root', 
                      password='MarianHighSchool402$)@', 
                      dbname='marian', host='34.135.251.192')
      dbBegin(db)
      
      dbWriteTable(db, input$radio, data_to_upload, append = TRUE, overwrite = FALSE)
      dbCommit(db)  # Commit the transaction
      output$upload_status <- renderText("Data Uploaded Successfully!")
      dbDisconnect(db)
    }, 
    error = function(e) {
      dbRollback(db)  # Rollback the transaction on error
      output$upload_status <- renderText(sprintf("Failed to upload data: %s", e$message))
      dbDisconnect(db)
    })
  })
}

shinyApp(ui = ui, server = server)