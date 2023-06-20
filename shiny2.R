library(shiny)


# Define UI for app that draws a histogram ----
library(ggplot2)

#PBP Data
pbp_data <- read.csv("C:\\Users\\BrocFassnacht\\OneDrive - Cobbs Creek Healthcare\\Desktop\\Training\\R Training\\GitR\\pbp2022.csv")
last_p <- filter(pbp_data, last.pitch.of.ab == "true")

ui <- fluidPage(
  titlePanel("Basic DataTable"),
  
  # Create a new Row in the UI for selectInputs
  fluidRow(
    column(4,
           selectInput("Pitch",
                       "Pitch:",
                       c("All",
                         unique(last_p$details.type.description)))
    )
  ),
  # Create a new row for the table.
  DT::dataTableOutput("table")
)

server <- function(input, output) {
  
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    data <- mpg
    if (input$man != "All") {
      data <- data[data$details.type.description == input$details.type.description,]
    }
    data
  }))
  
}

shinyApp(ui = ui, server = server)