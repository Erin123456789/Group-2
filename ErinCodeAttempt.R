
library(RMariaDB)
library(DBI)
library(shiny)
library(bslib)

db <- dbConnect(RMariaDB::MariaDB(), user='root', password='MarianHighSchool402$)@', dbname='marian', host='34.135.251.192')

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
          submitButton("Submit"),
          actionButton("Cancel" ,label="Cancel")
            ),
        card(
          radioButtons(
            "radio",
            "Select option",
            choices = list("ACT Data" = 1, "PreACT Data" = 2, "PreACT 8/9 Data" = 3, "PreSAT Data" = 4),
            selected = 1
             )
           ),
    
    ),
    tableOutput(
      "contents"
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
  #GOAL - make these selections classify the data under the correct test type
    #switch(input$radio,
           #"1" = df[, db_ACT, drop = FALSE],      # ACT Data
           #"2" = df[, db_PreACT, drop = FALSE],   # PreACT Data
           #"3" = df[, db_PreACT89, drop = FALSE], # PreACT 8/9 Data
           #"4" = df[, db_PreSAT, drop = FALSE]    # PreSAT Data
    #)
  
  
  
  
}

# Run the app ----
shinyApp(ui = ui, server = server)

