# shinyServer.R

# Packages Needed
library(shiny)
library(data.table)
library(damr)
library(ggetho)
library(DT)
library(sleepr)
source("helpers.R")

# Increase upload size
options(shiny.maxRequestSize=30*1024^2)

server <- function(input, output, session) {

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

  })



  ############## process data tab

  create_sleep_plot <- function(data) {
    # pdf(filename)
    sleep_plot <- ggetho(data, aes(z = asleep)) +
      stat_ld_annotations(ypos = "top") +
      stat_tile_etho()
    # print(sleep_plot)
    # dev.off()
    # message(message_text)
    return(sleep_plot)
  }
  
  observe({

    req(input$meta_file)
    metadata <- read.csv(input$meta_file$datapath)
    metadata <- na.omit(metadata)

    updateSelectInput(session, "plot_choice", choices = unique(metadata$monitor))

    fixUploadedFilesNames <- function(x) {
      if (is.null(x)) {
        return()
      }

      oldNames <- x$datapath
      newNames <- file.path(
        dirname(x$datapath),
        x$name
      )
      file.rename(from = oldNames, to = newNames)
      x$datapath <- newNames
      x
    }

    file.copy(fixUploadedFilesNames(input$monitor_files)$datapath, ".", recursive = TRUE, overwrite = TRUE)
    metadata_proc <- link_dam_metadata(metadata, result_dir = ".")
    output$contents <- DT::renderDataTable(
      metadata,
      filter = list(position = "top", clear = FALSE, plain = TRUE)
    )

    observeEvent(input$save_process, {
      withBusyIndicatorServer("save_process", {
        
        req(input$batch_title)
        batch_title <- input$batch_title

        dt_sleep <- load_dam(metadata_proc, FUN = sleepr::sleep_dam_annotation)
        dt_act <- load_dam(metadata_proc)

        dt_curated_1 <- curate_dead_animals(dt_sleep)
        removed_ids <- setdiff(dt_sleep[, id, meta = TRUE], dt_curated_1[, id, meta = TRUE])

        curated_1_list <- data.table(removed_ids)
        write.csv(curated_1_list, paste0("generated_files/removed_list1_", batch_title, ".csv"))  

        #TODO continue processing...
        
      #   create_sleep_plot(dt_sleep, paste0("generated_files/", batch_title, "_sleep_before_deadcheck.pdf"), paste0(batch_title, "sleep_before_deadcheck.pdf written"))
      #   create_sleep_plot(dt_curated_1, paste0("generated_files/", batch_title, "_sleep_after_deadcheck.pdf"), paste0(batch_title, "sleep_after_deadcheck.pdf written"))
      })

      output$status <- renderText({
        req(input$save_process)
        "Pre-Processing Complete!"
      })
      
      #################

      observeEvent(input$plot_button, {
        output$plots_output <- renderPlot({
          req(input$meta_file, input$plot_choice, input$plot_type)

          selected_monitor <- input$plot_choice
          plot_type <- input$plot_type
          plot_data <- metadata[metadata$monitor == selected_monitor, ]

          if (plot_type == "Activity Plot") {

            act_etho <- ggetho(dt_act[xmv(monitor) == selected_monitor], aes(z = activity)) +
              stat_bar_tile_etho() +
              scale_x_days()
            print(act_etho)
          }

          if (plot_type == "Sleep Plot") {

            sleep_etho <- ggetho(dt_sleep[xmv(monitor) == selected_monitor], aes(z = asleep)) +
              stat_bar_tile_etho() +
              scale_x_days()
            print(sleep_etho)
          }

        })
      })

      observeEvent(input$deadcheck_display, {
        req(input$deadcheck_display)

        output$before_dead <- renderPlot({
          create_sleep_plot(dt_sleep)
        })

        output$after_dead <- renderPlot({
          create_sleep_plot(dt_curated_1)
        })

        # Handle PDF downloads
        observeEvent(input$download_deadcheck, {
          
          filename <- paste0("generated_files/", batch_title, "_sleep_before_deadcheck.pdf")
          pdf(filename)
          print(create_sleep_plot(dt_sleep))
          dev.off()
          
        })

        observeEvent(input$download_deadcheck, {

          filename <- paste0("generated_files/", batch_title, "_sleep_after_deadcheck.pdf")
          pdf(filename)
          print(create_sleep_plot(dt_curated_1))
          dev.off()

        })

      })

    

    })
  })


}
