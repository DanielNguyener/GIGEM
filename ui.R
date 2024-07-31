# ui.R

# User Interface
library(shiny)
library(DT)

ui <- fluidPage(
  navbarPage(
    title = "Prototype",
    position = c("static-top"),
    fluid = TRUE,
    collapsible = TRUE,

    tabPanel(
      title = "Metadata Input",
      
      fileInput("file1", "Choose Text Files", multiple = TRUE, accept = c("text/plain", ".txt")),
      
      h3("Enter Metadata"),
      uiOutput("metadata_inputs"),
      
      h3("Saved Files Metadata"),
      tableOutput("saved_files"),
      
      actionButton("save_csv", "Save Metadata as CSV"),

      h3("Omit Monitor & Region_ID Combinations"),
      uiOutput("omit_rows")
    ),

    tabPanel(
      title = "Process Data",
      sidebarPanel(
        fileInput("monitor_files", "Upload monitor.txt files", multiple = TRUE, accept = c(".txt")),
        fileInput("meta_file", "Upload metadata.csv", multiple = FALSE, accept = (c(".csv"))),
        textInput("batch_title", "Batch Title"),
        
        actionButton("save_process", "Save"),
      ),

      mainPanel(
        tabsetPanel(
          tabPanel("Processed Data", DT::dataTableOutput("meta_contents")),
          tabPanel("Activity Plot Selection",
            selectInput("monitor1_select", "Select Monitor 1", choices = NULL),
            selectInput("monitor2_select", "Select Monitor 2", choices = NULL),
            actionButton("plot_button", "View Activity Plots"),
            uiOutput("plots_output"),
            downloadButton("download_all_plots", "Download All Plots as PNG"),
 
          ),
        )
      )
    )
  )
  
)