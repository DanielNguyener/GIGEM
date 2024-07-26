# ui.R

# User Interface
library(shiny)

ui <- fluidPage(
  titlePanel("Prototype"),
  
  sidebarLayout(
    sidebarPanel(
      width = 2,
      fileInput(
        "data",
        "Choose Monitor Files",
        multiple = TRUE,
        accept = c(
          "text/csv",
          "text/comma-separated-values,text/plain",
          ".csv"
        )
      )
    ),
    
    mainPanel(
      # Main content can go here if needed
    )
  )
)
