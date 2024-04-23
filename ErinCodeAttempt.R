
library(RMariaDB)
library(DBI)
library(shiny)
library(bslib)

db <- dbConnect(RMariaDB::MariaDB(), 
                user='root', 
                password='MarianHighSchool402$)@', 
                dbname='marian', host='34.135.251.192')

#create a list of the column names
database_columns <- dbGetQuery(db, "DESCRIBE marian.ACT")
db_ACT <- database_columns$Field
database_columns2 <- dbGetQuery(db, "DESCRIBE marian.PreACT")
db_PreACT <- database_columns2$Field
database_columns3 <- dbGetQuery(db, "DESCRIBE marian.PreACT89")
db_PreACT89 <- database_columns3$Field
database_columns4 <- dbGetQuery(db, "DESCRIBE marian.PreSAT")
db_PreSAT <- database_columns4$Field

#UI
ui <- fluidPage (
  titlePanel("Standardized Testing Data"),
  sidebarLayout(
    sidebarPanel(
        card(
          card_header("File input"),
          fileInput("file", label = NULL),
          actionButton("upload", label="Upload")
            ),
        card(
          radioButtons(
            "radio",
            "Select option",
            choices = list("ACT Data" = 1, "PreACT Data" = 2, "PreACT 8/9 Data" = 3, "PreSAT Data" = 4),
            selected = 1)
        ),
    ),
    mainPanel(
    tableOutput(
      "contents"),
    uiOutput("column_select_ui"),
    uiOutput("database_column_select_ui")
    )
  ),
  )



# Define server logic ----
server <- function(input, output) {
  #data preview
  selected_file_data <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
  })
    # Render data preview table
  output$contents <- renderTable({
    req(selected_file_data())
    head(selected_file_data(), 10) # Display first 10 rows
  })

  #Will upload the data to the server, at some point
  {v <- reactiveValues(data = NULL)
    observeEvent(input$submit, {
      v$data <- submit(100)
    })}
  
  #CODE FOR THE RADIO BUTTON
  observeEvent(input$radio, {
    if(input$radio == 1 ) {
      output$database_column_select_ui <- renderUI({
        selectInput("database_column","Select Database Column:", choices = db_ACT)
          ACT <- c("student_id","test_date","district","score","new_pred_act")
        columnnames <- colnames(ACT)
        output$column <- renderUI({
          selectInput("columnnames", label = "Select Database Column:", choices = columnnames)
        })
      })
    }
    if(input$radio == 2) {
      output$database_column_select_ui <- renderUI({
        selectInput("database_column","Select Database Column:", choices = db_PreACT)
        PreACT <- c("student_id","test_date","district","score","new_pred","pred_act")
        columnnames <- colnames(PreACT)
        output$column <- renderUI({
          selectInput("columnnames", label = "Select Database Column:", choices = columnnames)
        })
      })
      }
    if(input$radio == 3) {
      output$database_column_select_ui <- renderUI({
        selectInput("database_column","Select Database Column:", choices = db_PreACT89)
        })}
    else{
      output$database_column_select_ui <- renderUI({
        selectInput("database_column", "Select Database Column:", choices = db_PreSAT)
        })}
    })
  
  #READ THE SELECTED FILE AND EXTRACT COLUMN NAMES
  #THEN, IN THE SAME EVENT, GENERATE A SELECT INPUT WIDGET FOR THE UPLOAD FILE SELECTION
  {
    observeEvent(input$file, {req(input$file)
      df <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
      columnnames <- colnames(df)
      output$column_select_ui <- renderUI({
      selectInput("column_names", label = "Select Your Columns", choices = columnnames)})
  })
  }
  
  #UPLOAD DATA INTO THE DATABASE
  {
    insert_data <- function(data, db) {
      dbWriteTable(db, "your_table", data, append = TRUE)
    }
    observeEvent(input$upload, {
      if(!is.null(input$file)) {
        df <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
        insert_data(df,db)
        output$upload_status <- renderText ({
          "Data Uploaded Successfully!"})
      }
        })
      }
    }
  

# Run the app ----
shinyApp(ui = ui, server = server)

