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

  # pre-defined FUNCTIONS
  create_sleep_plot <- function(data) {

    sleep_plot <- ggetho(data, aes(z = asleep)) +
      stat_ld_annotations(ypos = "top") +
      stat_tile_etho()

    return(sleep_plot)
  }

  create_population_plot <- function(plot_data, wrap_time = NULL) {

    pop_sleep_plot <- ggetho(plot_data, aes(y = asleep, colour = treatment), time_wrap = wrap_time) +
      stat_pop_etho() +
      stat_ld_annotations() +
      facet_grid(genotype ~ .) +
      scale_y_continuous(name = "Fraction of time sleeping", labels = scales::percent)
    
    return(pop_sleep_plot)
  }

  ZTbins <- function(dt, summary_dt_final, selected_bins) {
    
    # Convert selected bins to start hours
    bin_hours <- as.numeric(gsub("ZT_(\\d+)_\\d+", "\\1", selected_bins))
    
    # Initialize list to store summary data
    summaries <- list()
    
    # Process each selected bin
    for (start_hour in bin_hours) {
      end_hour <- start_hour + 4
      start_time <- sprintf("%04d", start_hour * 3600)  # Start time in seconds
      end_time <- sprintf("%04d", end_hour * 3600)    # End time in seconds
      
      # Create bin-specific data
      dt_sleep <- dt[t %between% c(start_time, end_time)]
      
      # Create new columns for sleep fraction and sleep time
      summary_dt_sleep <- dt_sleep[, .(
        sleep_fraction = mean(asleep),
        sleep_time = 240 * mean(asleep)
      ), by = id]
      
      # Prepare new column names
      new_col_names <- c(
        "id", 
        paste0("sleep_fraction_ZT", start_hour, "_", end_hour),
        paste0("sleep_time_ZT", start_hour, "_", end_hour)
      )
      
      # Ensure the number of new column names matches the number of columns
      if (ncol(summary_dt_sleep) == length(new_col_names)) {
        setnames(summary_dt_sleep, old = names(summary_dt_sleep), new = new_col_names)
      } else {
        message("Mismatch in column names:\n")
        message("Expected names:", length(new_col_names), "\n")
        message("Actual names:", ncol(summary_dt_sleep), "\n")
        stop("Column count mismatch. Please check the renaming logic.")
      }
      
      # Rename columns in summary_dt_final to prevent conflicts
      final_col_names <- names(summary_dt_final)
      conflict_cols <- intersect(new_col_names[-1], final_col_names)  # Exclude "id" from conflict check
      if (length(conflict_cols) > 0) {
        new_final_names <- make.unique(final_col_names)
        setnames(summary_dt_final, old = final_col_names, new = new_final_names)
      }
      
      # Merge summaries
      summary_dt_final <- merge(summary_dt_final, summary_dt_sleep, by = "id", all.x = TRUE)
      
      # Store the summary data in the list
      summaries[[paste0("ZT_", start_hour, "_", end_hour)]] <- summary_dt_sleep
    }
    
    return(summary_dt_final)
  }

  process_days <- function(num_days, bout_dt, summary_dt_final) {
    for (day in 1:num_days) {
      start_time <- days(day - 1)
      end_time <- start_time + hours(12)
      
      # Extracting the bouts for the current day
      bout_dt_current_day <- bout_dt[t %between% c(start_time, end_time)]
      bout_dt_current_day[, t := t - start_time]
      
      # Summary for the current day
      bout_summary <- bout_dt_current_day[, .(
        latency = t[1],
        first_bout_length = duration[1],
        latency_to_longest_bout = t[which.max(duration)]
      ), by = id]
      
      # Renaming columns for the current day
      setnames(bout_summary, c("latency", "first_bout_length", "latency_to_longest_bout"),
        c(paste0("Day", day, "_latency"), paste0("Day", day, "_first_bout_length"), paste0("Day", day, "_latency_to_longest_bout")))
      
      # Merging with the final summary data table
      summary_dt_final <- merge(summary_dt_final, bout_summary, by = "id")
    }
  return(summary_dt_final)
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
        
        req(input$batch_title, input$num_days)
        message(paste("Calculating... Please Wait."))
        batch_title <- input$batch_title
        num_days <- input$num_days

        dt_sleep <- load_dam(metadata_proc, FUN = sleepr::sleep_dam_annotation)
        dt_act <- load_dam(metadata_proc)

        dt_curated_1 <- curate_dead_animals(dt_sleep)
        removed_ids <- setdiff(dt_sleep[, id, meta = TRUE], dt_curated_1[, id, meta = TRUE])

        curated_1_list <- data.table(removed_ids)
        write.csv(curated_1_list, paste0("generated_files/removed_list1_", batch_title, ".csv"))
        message(paste("removed_list1_", batch_title, ".csv written"))

        lifespan_dt <- dt_curated_1[, .(lifespan = max(t)), by=id]
        valid_ids <- lifespan_dt[lifespan > days(num_days), id]

        dt_curated_2 <- dt_curated_1[id %in% valid_ids]

        removed_ids_2 <- setdiff(dt_curated_1[, id, meta = TRUE], dt_curated_2[, id, meta = TRUE])
        curated_2_list <- data.table(removed_ids_2)
        write.csv(curated_2_list, paste0("generated_files/removed_list2_", batch_title, ".csv"))
        message(paste("removed_list2_", batch_title, ".csv written"))

        dt_curated_final <- dt_curated_2[t %between% c(days(0), days(num_days))]
        
        dt <- behavr(dt_curated_final, metadata_proc)

        summary_dt_final <- rejoin(dt[, .(sleep_fraction = mean(asleep)), by = id])

        dt[, phase := ifelse(t %% hours(24) < hours(12), "L", "D")]

        message("Calculations complete!")
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

        before <- create_sleep_plot(dt_sleep)
        after <- create_sleep_plot(dt_curated_1)

        output$before_dead <- renderPlot({
          before
        })

        output$after_dead <- renderPlot({
          after
        })

        # Handle PDF downloads
        observeEvent(input$download_deadcheck, {
          
          filename <- paste0("generated_files/", batch_title, "_sleep_before_deadcheck.pdf")
          pdf(filename)
          print(before)
          dev.off()
          
          filename <- paste0("generated_files/", batch_title, "_sleep_after_deadcheck.pdf")
          pdf(filename)
          print(after)
          dev.off()

          showModal(modalDialog(
            title = "Download Complete",
            paste0("downloaded as generated_files/", batch_title, "_sleep_before_deadcheck.pdf and
            generated_files/", batch_title, "_sleep_after_deadcheck.pdf"),
            easyClose = TRUE,
            footer = NULL
          ))

        })
      })

      observeEvent(input$sleep_pop_display, {
        req(input$sleep_pop_display)

        sleeppop <- create_population_plot(dt_curated_final)
        sleeppop_wrap <- create_population_plot(dt_curated_final, wrap_time = hours(24))

        output$sleep_pop <- renderPlot({
          sleeppop
        })

        output$sleep_pop_wrap <- renderPlot({
          sleeppop_wrap
        })

        observeEvent(input$download_sleeppop, {
          filename <- paste0("generated_files/", batch_title, "_population_sleep.pdf")
          pdf(filename)
          print(sleeppop)
          dev.off()

          filename <- paste0("generated_files/", batch_title, "_population_sleep_wrap.pdf")
          pdf(filename)
          print(sleeppop_wrap)
          dev.off()

          showModal(modalDialog(
            title = "Download Complete",
            paste0("downloaded as generated_files/", batch_title, "_population_sleep.pdf and
            generated_files/", batch_title, "_population_sleep_wrap.pdf"),
            easyClose = TRUE,
            footer = NULL
          ))

        })
      })

      observeEvent(input$cal_sum, {
        
        summary_dt_final <- rejoin(dt[, .(
          sleep_fraction = mean(asleep),
          sleep_fraction_all = mean(asleep),
          sleep_time_all = 1440 * mean(asleep),
          sleep_fraction_l = mean(asleep[phase == "L"]),
          sleep_time_l = 720 * mean(asleep[phase == "L"]),
          sleep_fraction_d = mean(asleep[phase == "D"]),
          sleep_time_d = 720 * mean(asleep[phase == "D"])
        ), by = id])

        observeEvent(input$zt_bins, {
          
          message(paste("Creating bins..."))
          summary_dt_final <- ZTbins(dt, summary_dt_final, input$zt_bins) # Duplicated column names.

          message(paste("Bout calculations..."))
          bout_dt <- bout_analysis(asleep, dt)
          bout_dt <- bout_dt[asleep == TRUE, -"asleep"]

          message(paste("Creating plot..."))
          sleep_bout_wrap <- ggetho(bout_dt, aes(y = duration / 60, colour = treatment), time_wrap = hours(24)) +
            stat_pop_etho() +
            facet_grid(genotype ~ .) +
            scale_y_continuous(name = "Bout length (min)")
    

          output$sleep_bout_wrap <- renderPlot({
            sleep_bout_wrap
          })

          observeEvent(input$download_sleeppopbout, {
            filename <- paste0("generated_files/", batch_title, "_population_sleep_bout_wrap.pdf")
            pdf(filename)
            print(sleep_bout_wrap)
            dev.off()
            showModal(modalDialog(
              title = "Download Complete",
              paste0("downloaded as generated_files/", batch_title, "_population_sleep_bout_wrap.pdf"),
              easyClose = TRUE,
              footer = NULL
            ))
          })

          message(paste("Processing Latency & Bout Lengths..."))

          # DEFINE PROCESS_DAYS FUNCTION
          summary_dt_final <- process_days(num_days, bout_dt, summary_dt_final)  ### duplicated column names.

          bout_dt_min <- bout_analysis(asleep, dt)[, .(
            id, duration = duration / 60, t = t / 60,
            phase = ifelse(t %% hours(24) < hours(12), "L", "D")
          )][duration >= 5]
          
          bout_dt_min_L <- bout_dt_min[phase == "L"]
          bout_dt_min_D <- bout_dt_min[phase == "D"]

          summary_bout_L <- bout_dt_min_L[, .(
            n_bouts_L = .N / num_days,
            mean_bout_length_L = mean(duration)
          ), by = id]
          
          summary_bout_D <- bout_dt_min_D[, .(
            n_bouts_D = .N / num_days,
            mean_bout_length_D = mean(duration)
          ), by = id]
          
          # merge information
          summary_dt_final <- merge(summary_dt_final, summary_bout_L[, .(id, n_bouts_L, mean_bout_length_L)], by = "id")
          summary_dt_final <- merge(summary_dt_final, summary_bout_D[, .(id, n_bouts_D, mean_bout_length_D)], by = "id")
          summary_dt_final <- summary_dt_final[, -2]

          observeEvent(input$download_sum, {
            write.csv(summary_dt_final, file = paste0("generated_files/summary_", batch_title, ".csv"), row.names = FALSE)

            showModal(modalDialog(
              title = "Download Complete",
              paste0("downloaded as generated_files/summary_", batch_title, ".csv"),
              easyClose = TRUE,
              footer = NULL
            ))
          })

          message("Summary table written!")
        })
      })
      

    })
  })

  observe({
    zt_bins_selected <- input$zt_bins
    
    # Create dynamic choices based on selected ZT bins
    additional_choices <- sapply(zt_bins_selected, function(bin) {
      paste(bin)  # Create a label for each bin
    }, simplify = FALSE)
    
    # Convert additional_choices to a named vector if needed
    additional_choices_named <- setNames(
      paste(zt_bins_selected),
      zt_bins_selected
    )
    
    # Update choices for 'groups'
    updateCheckboxGroupInput(
      session,
      "groups",
      choices = c(
        "Sleep Time All" = "sleep_time_all",
        "Sleep Time L" = "sleep_time_l",
        "Sleep Time D" = "sleep_time_d",
        "N Bouts L" = "n_bouts_L",
        "N Bouts D" = "n_bouts_D",
        "Mean Bout Length L" = "mean_bout_length_L",
        "Mean Bout Length D" = "mean_bout_length_D",
        additional_choices_named
      )
    )
  })


}
