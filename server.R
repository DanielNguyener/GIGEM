# shinyServer.R

# Packages Needed

library(shiny)
options(shiny.maxRequestSize=30*1024^2)

server <- function(input, output, session) {
  # Reactive value to keep track of file names
  files <- reactiveVal(c())
  
  observeEvent(input$createFile, {
    fileName <- input$fileName
    
    # Check if file name is not empty and is not already in the list
    if (nzchar(fileName) && !fileName %in% files()) {
      
      # append to list of file names.
      file.create(fileName)
      
      # Update the list of files
      files(c(files(), fileName))
    }
  })
  
  output$fileList <- renderUI({
    fileNames <- files()
    
    if (length(fileNames) > 0) {
      tags$ul(
        lapply(fileNames, function(name) {
          tags$li(name)
        })
      )
    } else {
      "No files added yet."
    }
  })
  
  
  
  
  
  
  
  
}