# shinyServer.R

# Packages Needed
library(shiny)
library(data.table)
library(damr)
library(ggetho)
library(DT)

# Increase upload size
options(shiny.maxRequestSize=30*1024^2)

server <- function(input, output, session) {
  # Reactive value to store uploaded file names and metadata
  uploaded_files <- reactiveVal(NULL)
  metadata <- reactiveVal(data.frame())

  observe({
  
    observeEvent(input$file1, {
      req(input$file1)
      files <- input$file1
      num_files <- nrow(files)
      
      output$metadata_inputs <- renderUI({
        tagList(
          tags$table(
            tags$tr(
              tags$th("File Name"),
              tags$th("Genotype"),
              tags$th("Treatment"),
              tags$th("Temperature (C)"),
              tags$th("Sex"),
              tags$th("Start Date"),
              tags$th("Start Time (HH:MM:SS)"),
              tags$th("Stop Date"),
              tags$th("Stop Time (HH:MM:SS)")
            ),
            lapply(1:num_files, function(i) {
              tags$tr(
                tags$td(files$name[i]),
                tags$td(textInput(paste0("genotype_", i), NULL)),
                tags$td(textInput(paste0("treatment_", i), NULL)),
                tags$td(numericInput(paste0("temperature_", i), NULL, value = 25.0, step = 0.1)),
                tags$td(selectInput(paste0("sex_", i), NULL, choices = c("M", "F"))),
                tags$td(dateInput(paste0("start_date_", i), NULL, value = Sys.Date())),
                tags$td(textInput(paste0("start_time_", i), NULL, value = "00:00:00")),
                tags$td(dateInput(paste0("stop_date_", i), NULL, value = Sys.Date())),
                tags$td(textInput(paste0("stop_time_", i), NULL, value = "00:00:00"))
              )
            })
          ),
          actionButton("submit", "Submit")
        )
      })
    })
    
    observeEvent(input$submit, {
      req(input$file1)
      files <- input$file1
      num_files <- nrow(files)
      
  metadata_list <- lapply(1:num_files, function(i) {
    # Extract the monitor name from the file name
    file_name <- files$name[i]
    monitor_name <- paste0(substr(file_name, 1, 1), substr(file_name, 8, nchar(file_name) - 4))
    
    lapply(1:32, function(region_id) {
      # Combine date and time inputs and format as POSIXct
      start_date <- input[[paste0("start_date_", i)]]
      start_time <- input[[paste0("start_time_", i)]]
      stop_date <- input[[paste0("stop_date_", i)]]
      stop_time <- input[[paste0("stop_time_", i)]]
      
      start_datetime <- ifelse(
        is.na(start_time) || start_time == "",
        as.character(start_date),
        format(as.POSIXct(paste(start_date, start_time), format="%Y-%m-%d %H:%M:%S"), "%Y-%m-%d %H:%M:%S")
      )
      
      stop_datetime <- ifelse(
        is.na(stop_time) || stop_time == "",
        as.character(stop_date),
        format(as.POSIXct(paste(stop_date, stop_time), format="%Y-%m-%d %H:%M:%S"), "%Y-%m-%d %H:%M:%S")
      )
      
      list(
        file = files$name[i],
        monitor = monitor_name,
        region_id = as.integer(region_id),
        genotype = input[[paste0("genotype_", i)]],
        treatment = input[[paste0("treatment_", i)]],
        temp = as.numeric(input[[paste0("temperature_", i)]]),
        sex = input[[paste0("sex_", i)]],
        start_datetime = start_datetime,
        stop_datetime = stop_datetime
      )
    })
  })
      
      metadata_list <- do.call(rbind, lapply(metadata_list, function(x) do.call(rbind, x)))
      metadata_df <- as.data.frame(metadata_list)
      
      # Create a directory to save files if it does not exist
      if (!dir.exists("uploaded_files")) {
        dir.create("uploaded_files")
      }
      
      # Save the uploaded files to the server
      for (i in 1:num_files) {
        file_path <- file.path("uploaded_files", files$name[i])
        file.copy(files$datapath[i], file_path)
      }
      
      uploaded_files(metadata_df)
      
      # Display a confirmation message
      showModal(modalDialog(
        title = "Upload Complete",
        "Files and metadata have been successfully uploaded and saved.",
        easyClose = TRUE,
        footer = NULL
      ))
    })
    
    # Display the metadata table with formatting
    output$saved_files <- renderTable({
      req(uploaded_files())
      df <- uploaded_files()
      df$region_id <- as.integer(df$region_id)
      df$temp <- paste0(as.integer(df$temp), "°C")
      df
    })
    
    # Save the metadata table as CSV
    observeEvent(input$save_csv, {
      req(uploaded_files())
      df <- uploaded_files()
      # Convert all columns to character
      df[] <- lapply(df, as.character)
      write.csv(df, file = "generated_files/metadata.csv", row.names = FALSE)
      showModal(modalDialog(
        title = "Save Complete",
        "Metadata table has been saved as metadata.csv.",
        easyClose = TRUE,
        footer = NULL
      ))
    })

    # Render table for selection of rows to omit
    output$omit_rows <- renderUI({
      req(uploaded_files())
      df <- uploaded_files()
      tagList(
        checkboxGroupInput("rows_to_omit", "Select Monitor & Region_ID combinations to omit:",
                          choices = paste(df$monitor, df$region_id, sep = "_")),
        actionButton("omit", "Omit Selected Rows")
      )
    })
    
    # Render inputs for entering Monitor and Region_ID to omit
    output$omit_rows <- renderUI({
      tagList(
        textInput("monitor_to_omit", "Monitor to Omit:"),
        numericInput("region_id_to_omit", "Region ID to Omit:", value = 1, min = 1, step = 1),
        actionButton("omit", "Omit Selected Row")
      )
    })
    
    # Handle omitting rows
    observeEvent(input$omit, {
      req(uploaded_files())
      df <- uploaded_files()
      monitor <- input$monitor_to_omit
      region_id <- as.integer(input$region_id_to_omit)
      df <- df[!(df$monitor == monitor & df$region_id == region_id),]
      uploaded_files(df)
    })

    observeEvent(input$file2, {
      req(input$file2)
      metadata <- read.csv(input$file2$datapath)

      metadata_proc <- link_dam_metadata(metadata, result_dir = "./uploaded_files")
      output$meta_contents <- DT::renderDataTable(metadata, filter = list(position = "top", clear = FALSE, plain = TRUE))
    })

    observeEvent(input$plots, {
      req(uploaded_files(), metadata_proc)

      #load data
      dt <- load_dam(metadata_proc)

      for(i in unique(metadata_proc$monitor))
      {
        pdf_file <- paste0(ExperimentData@Batch,'_activity_by_monitor',i,'.pdf')
        pdf(pdf_file)
        
        # Create plot
        activity_by_monitor <-ggetho(dt[xmv(monitor) == i ], aes(z=activity)) +
          stat_bar_tile_etho() +
          scale_x_days()
        # Print plot to PDF
        print(activity_by_monitor)
        dev.off() # Close the PDF device
        print(paste0(pdf_file, ' written'))
      }

      

      showModal(modalDialog(
        title = "Preprocessing Complete", 
        "Data processing has been successfully completed.",
        easyClose = TRUE,
        footer = NULL
      ))
    })

  })
}
