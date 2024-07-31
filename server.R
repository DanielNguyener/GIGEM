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
  # Reactive value to store uploaded file names and metadata
  uploaded_files <- reactiveVal(NULL)
  metadata_proc <- reactiveVal(data.frame())
  batch_title <- reactiveVal()
  

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

      # Combine lists into a single data frame
      metadata_list <- do.call(rbind, lapply(metadata_list, function(x) do.call(rbind, x)))
      metadata_df <- as.data.frame(metadata_list, stringsAsFactors = FALSE)
      
      # Create a directory to save files if it does not exist
      if (!dir.exists("uploaded_files")) {
        dir.create("uploaded_files")
      }

      # Save the uploaded files to the server
      file_paths <- character(num_files)
      for (i in 1:num_files) {
        file_path <- file.path("uploaded_files", files$name[i])
        file.copy(files$datapath[i], file_path)
        file_paths[i] <- file_path
      }

      uploaded_files(list(metadata = metadata_df, file_paths = file_paths))
      
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
      df <- uploaded_files()$metadata
      df$region_id <- as.integer(df$region_id)
      df$temp <- paste0(as.integer(df$temp), "°C")
      df
    })
    
    # Save the metadata table as CSV
    observeEvent(input$save_csv, {
      req(uploaded_files())
      df <- uploaded_files()$metadata
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


    # Render inputs for entering Monitor and Region_ID to omit
    output$omit_rows <- renderUI({
      req(uploaded_files())
      df <- uploaded_files()$metadata
      tagList(
        textInput("monitor_to_omit", "Monitor to Omit:"),
        numericInput("region_id_to_omit", "Region ID to Omit:", value = 1, min = 1, step = 1),
        actionButton("omit", "Omit Selected Row")
      )
    })

    
    # Handle omitting rows
    observeEvent(input$omit, {
      req(uploaded_files())
      df <- uploaded_files()$metadata
      monitor <- input$monitor_to_omit
      region_id <- as.integer(input$region_id_to_omit)
      
      if (!is.na(monitor) && !is.na(region_id)) {
        df <- df[!(df$monitor == monitor & df$region_id == region_id), ]
        uploaded_files(list(metadata = df, file_paths = uploaded_files()$file_paths))
      }
    })



    ############## process data tab

    observeEvent(input$save_process, {
      req(input$monitor_files)
      req(input$meta_file)
      req(input$batch_title)

      metadata <- read.csv(input$meta_file$datapath)
      batch_title <- input$batch_title

      files <- input$monitor_files
      num_files <- nrow(files)

      # # Create a directory to save files if it does not exist
      # if (!dir.exists("uploaded_files")) {
      #   dir.create("uploaded_files")
      # }

      # # Save the uploaded files to the server
      # file_paths <- character(num_files)
      # for (i in 1:num_files) {
      #   file_path <- file.path("uploaded_files", files$name[i])
      #   file.copy(files$datapath[i], file_path)
      #   file_paths[i] <- file_path
      # }

      showModal(modalDialog(
        title = "Saved data",
        easyClose = TRUE,
        footer = NULL
      ))

      metadata_proc(link_dam_metadata(metadata, result_dir = "uploaded_files"))
      output$meta_contents <- DT::renderDataTable(metadata, filter = list(position = "top", clear = FALSE, plain = TRUE))
    })
      
    # Update select inputs for monitors
    observe({
      req(metadata_proc())
      monitors <- unique(metadata_proc()$monitor)
      updateSelectInput(session, "monitor1_select", choices = monitors)
      updateSelectInput(session, "monitor2_select", choices = monitors)
    

    observeEvent(input$plots, {
      output$plots_output <- renderUI({
        tagList(
          plotOutput("plot_monitor1", height = 600, width = 800),
          plotOutput("plot_monitor2", height = 600, width = 800)
        )
      })
      
      output$plot_monitor1 <- renderPlot({
        req(input$monitor1_select)
        dt <- load_dam(metadata_proc())
        ggetho(dt[xmv(monitor) == input$monitor1_select], aes(z = activity)) +
          stat_bar_tile_etho() +
          scale_x_days()
      })
      
      output$plot_monitor2 <- renderPlot({
        req(input$monitor2_select)
        dt <- load_dam(metadata_proc())
        ggetho(dt[xmv(monitor) == input$monitor2_select], aes(z = activity)) +
          stat_bar_tile_etho() +
          scale_x_days()
      })
    })
    })

    output$download_all_plots <- downloadHandler(
      filename = function() {
        paste("activity_", Sys.Date(), ".zip")
      },
      content = function(file) {
        # ensure directory exists,
        dir.create("generated_files", showWarnings = FALSE)

        # get unique monitors
        monitors <- unique(metadata_proc()$monitor)
        dt <- load_dam(metadata_proc())

        for (monitor in monitors) {
          p <- ggetho(dt[xmv(monitor) == monitor], aes(z = activity)) +
            stat_bar_tile_etho() +
            scale_x_days()
          ggsave(filename = file.path("generated_files", paste0(batch_title, "_activity_by_", monitor, ".png")), plot = p)
        }

        zip(file, files = list.files("generated_files", full.names = TRUE))
      }
    )
  })
}
