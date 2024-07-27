# ui.R

# User Interface
library(shiny)

ui <- fluidPage(
  titlePanel("Upload Multiple Text Files with Metadata"),
  
  fileInput("file1", "Choose Text Files", multiple = TRUE, accept = c("text/plain", ".txt")),
  actionButton("submit", "Submit"),
  
  h3("Enter Metadata"),
  uiOutput("metadata_inputs"),
  
  h3("Saved Files Metadata"),
  tableOutput("saved_files")
)