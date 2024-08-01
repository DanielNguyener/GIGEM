# ui.R

# User Interface
library(shiny)
library(DT)
source("helpers.R")

ui <- fluidPage(
  use_busy_spinner(),
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
      
        withBusyIndicatorUI(actionButton("save_process", "Save")),
        withBusyIndicatorUI(textOutput("status"))
        
        
        
      ),

      mainPanel(
        tabsetPanel(
          tabPanel("Metadata", DT::dataTableOutput("contents")),
          tabPanel("Activity Plots",
            selectInput("plot_choice", "Choose Monitor", choices = NULL),
            selectInput("plot_type", "Choose Plot Type", choices = c("Activity Plot", "Sleep Plot")),
            actionButton("plot_button", "View Activity Plots"),
            plotOutput("plots_output"),
          ),
        )
      )
    )
  )
)