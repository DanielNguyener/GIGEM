# ui.R

# User Interface
library(shiny)
library(DT)
source("helpers.R")

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
        numericInput("num_days", "Num Days:", value = 1, min = 1, max = 100, step = 1),

        p("Please wait until this process finishes before displaying or downloading any plots."),
        withBusyIndicatorUI(actionButton("save_process", "Save")),
        withBusyIndicatorUI(textOutput("status"))
        
        
        
      ),

      mainPanel(
        tabsetPanel(
          tabPanel("Metadata", DT::dataTableOutput("contents")),
          tabPanel("Activity Plots",
            p("select which monitor, and plot type to display."),
            selectInput("plot_choice", "Choose Monitor", choices = NULL),
            selectInput("plot_type", "Choose Plot Type", choices = c("Activity Plot", "Sleep Plot")),
            actionButton("plot_button", "View Activity Plots"),
            plotOutput("plots_output"),
          ),
          tabPanel("Deadcheck Plots",
            p("before you can download, you must click show plots."),
            actionButton("deadcheck_display", "Show Deadcheck Plots"),
            h4("Before Deadcheck"),
            plotOutput("before_dead"),
            h4("After Deadcheck"),
            plotOutput("after_dead"),
            actionButton("download_deadcheck", "Download PDFs"),
          ),
          tabPanel("SleepPop Plots",
            p("before you can download, you must click show plots."),
            actionButton("sleep_pop_display", "Show Sleep Population Plots"),
            plotOutput("sleep_pop"),
            plotOutput("sleep_pop_wrap"),
            actionButton("download_sleeppop", "Download PDFs"),
          ),

          tabPanel("Create Summary",
            p("1. Create desired ZT bins."),
            checkboxGroupInput(
              inputId = "zt_bins",
              label = "Select ZT Bins:",
              choices = c(
                "ZT 0-4" = "ZT0_4",
                "ZT 4-8" = "ZT4_8",
                "ZT 8-12" = "ZT8_12",
                "ZT 12-16" = "ZT12_16",
                "ZT 16-20" = "ZT16_20",
                "ZT 20-24" = "ZT20_24"
              ),
            ),
            p("2. sleep fraction & time for: all, light, dark and all additional bins desired."),
            p("3. latency & bout values for each day."),
            actionButton("cal_sum", "Calculate Summary!"),
            actionButton("download_sum", "Download Summary CSV"),
            plotOutput("sleep_bout_wrap"),
            actionButton("download_sleeppopbout", "Download PDF"),
          
            checkboxGroupInput(
              inputId = "groups",
              label = "Select Groups to Analyze:",
              choices = c("Sleep Time All" = "sleep_time_all",
                          "Sleep Time L" = "sleep_time_l",
                          "Sleep Time D" = "sleep_time_d",
                          "N Bouts L" = "n_bouts_L",
                          "N Bouts D" = "n_bouts_D",
                          "Mean Bout Length L" = "mean_bout_length_L",
                          "Mean Bout Length D" = "mean_bout_length_D",
                          "")
            ),
            
            checkboxGroupInput(
              inputId = "treatments",
              label = "Treatments:",
              choices = c(""),
            ),
            checkboxGroupInput(
              inputId = "genotypes",
              label = "Genotypes:",
              choices = c(""),
            ),

            actionButton("norm_summary", "Create Normalized Values"),
            actionButton("down_norm_sum", "Download CSV"),
            br(),
            actionButton("stat_summary", "Create Statistics"),
            actionButton("down_stat_sum", "Download CSV"),
            br(),
            actionButton("norm_stat_summary", "Create Statistics for Normalized Values"),
            actionButton("down_norm_stat", "Download CSV"),
            br(),
            
          )
        )
      )
    )
  )
)