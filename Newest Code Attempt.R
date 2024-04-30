#Load Data ----
library(RMariaDB)
library(DBI)
library(shiny)
library(bslib)

db <- dbConnect(RMariaDB::MariaDB(), user='root', 
                password='MarianHighSchool402$)@', 
                dbname='marian',host='34.135.251.192')

table <- "ACT"
columns <- dbListFields(db, table)

#UI ----
ui <- fluidPage(
  titlePanel("standardized Testing Data"),
  sidebarLayout(
    sidebarPanel(
      card(
        card_header("Choose CSV File"),
        fileInput("file", label = NULL),
          #browse button
        actionButton("upload", label = "Upload")
          #upload button
        ),
      card(
        radioButtons(
          "radio", "Select Option", choices = list("ACT Data" = 1, "PreACT Data" = 2,
                                                   "PreACT89 Data" = 3, "PreSAT Data" = 4))
        ),
      ),
    mainPanel(
      tableOutput(
        "contents"),
      uiOutput("column_select_ui"),
      uiOutput("database_column_select_ui")
      )
    ),)

#Server ----
server <- function(input,output) {
#Data Preview
  selected_file_data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
    })
#Render Data Preview
  output$contents <- renderTable({
    req(selected_file_data())
    head(selected_file_data(), 10) #Display first 10 rows
    })
#Short-Hand Option for Radio Button
  observeEvent(input$radio, {
    output$database_column_select_ui <- renderUI({
    table <- listtable[input$radio]
    columns <- dbListFields(db, table)
    selectInput("columnnames", label = "Select Database Columns:", choices = columns)
      })
    })
#Generate select inputs for column
  data <- reactive ({
    req(input$file)
    read.csv(input$file$datapath)
    })
  output$column_mapping <- renderUI({
    req(data(),columns)
    lapply(columns, function(column){
      input[[column]]
    })
  })
}
#Run App ----
shinyApp(ui = ui, server = server)

