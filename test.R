library(shiny)
library(ggplot2)
library(dplyr)
library(ggethogram)  # Assuming ggethogram package is used

# Define UI for the app
ui <- fluidPage(
  titlePanel("Monitor Data Visualization"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("metadataFile", "Upload Metadata CSV", accept = ".csv"),
      textInput("monitorSelect", "Monitor ID", value = "M21"),
      actionButton("updateButton", "Update Plot")
    ),
    
    mainPanel(
      plotOutput("ethogramPlot")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive expression to read and process metadata
  processed_data <- reactive({
    req(input$metadataFile)
    
    metadata <- read.csv(input$metadataFile$datapath)
    metadata_proc <- link_dam_metadata(metadata, result_dir = "Monitor_files")
    load_dam(metadata_proc)
  })
  
  # Observe event when the button is clicked to update the plot
  observeEvent(input$updateButton, {
    req(processed_data())
    
    dt <- processed_data()
    monitor_select <- input$monitorSelect
    
    output$ethogramPlot <- renderPlot({
      ggetho(dt[xmv(monitor) == monitor_select], aes(z = activity)) +
        stat_bar_tile_etho() +
        scale_x_days()
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)