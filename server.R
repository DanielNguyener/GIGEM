# shinyServer.R

# Packages Needed
required_packages <- c("damr", "ggetho", "sleepr", "data.table", "zeallot", "plyr", "shiny", "DT", "behavr", "RColorBrewer", "ggplot2")

# Install and load required packages
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
  library(pkg, character.only = TRUE)
}

source("helpers.R")

# Increase upload size
options(shiny.maxRequestSize=30*1024^2)


server <- function(input, output, session) {

  uploaded_files <- reactiveVal(NULL) # this is updated in Metadata Input tab
  metadata_proc <- reactiveVal(data.frame()) # this is updated in Process Data tab
  batch_title <- reactiveVal() # this is updated in Process Data tab
  summary_dt_final <- reactiveVal() # this is only updated after generating ZT bins
  norm_factor <- reactiveVal(data.table()) # this is only generated after clicking generate normSummary
  dt_curated_final <- reactiveVal() # this will be used for sleep wrap plots

  
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

    output$saved_files <- DT::renderDataTable(
      uploaded_files()$metadata,
      filter = list(position = "top", clear = FALSE, plain = TRUE)
    )
    
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

      monitors_omit <- unique(df$monitor)
      tagList(
        # textInput("monitor_to_omit", "Monitor to Omit:"),
        selectInput("monitor_to_omit", "Monitor to Omit:", choices = monitors_omit),

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
    bin_hours <- as.numeric(gsub("ZT(\\d+)_\\d+", "\\1", selected_bins))
    
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

  generateNorms <- function(readin_summary_dt_final, normalized_factor, groups, geno, apply_genotype_filter, treatments, genotypes) {
    DT.list <- list()
    
    # Filter data based on selected treatments and genotypes
    filtered_data <- readin_summary_dt_final[treatment %in% treatments & genotype %in% genotypes]

    for (group in groups) {
      keep <- data.table()

      for (i in unique(filtered_data$genotype)) {
        genotype_subset <- filtered_data[genotype == i]

        if (apply_genotype_filter) {
          a <- normalized_factor[treatment %like% "Grp" & genotype == geno, ..group]
        } else {
          a <- normalized_factor[treatment %like% "Grp" & genotype == i, ..group]
        }
        
        if (nrow(a) > 0) {
          factor <- as.numeric(mean(a[[1]], na.rm = TRUE))  # Aggregate to single value
        } else {
          factor <- 1  # Default value if no data found
        }

        if (length(factor) != 1) {
          stop("Factor length mismatch")
        }

        new_col_name <- paste0("norm_", group)

        # Normalize with correct length
        genotype_subset[, (new_col_name) := .SD[[group]] / factor]
        
        keep <- rbind(keep, genotype_subset)
      }
      DT.list <- append(DT.list, list(keep))
    }
    return(DT.list)
  }

  generateSE <- function(data, groups, norm = FALSE) {
    #only add column for N values, once
    N_col <- FALSE
    if (norm){
      suffix <- "norm_"
    } else {
      suffix <- ""
    }
    
    # Filter data based on groups
    filtered_data <- data[, .SD, .SDcols = c("genotype", "treatment", paste0(suffix, groups))]
    
    # Loop through each group
    for (group in groups) {
      # Compute summary statistics for the current group
      summary_group <- summarySE(filtered_data, measurevar = paste0(suffix, group), groupvars = c("genotype", "treatment"))
      
      if (N_col) {
        summary_group_subset <- summary_group[, c(1, 2, 4:7)]
        summary_norm_common <- merge(summary_norm_common, summary_group_subset, by=c("genotype", "treatment"))
      } else {
        summary_norm_common <- summary_group
        N_col <- TRUE
      }
    }
    
    return(summary_norm_common)
  }

  # this function will generate both norm_summary and stat_norm
  # this function depends on both generateNorms and generateSE functions
  normSummary <- function(readin_summary_dt_final, groups, normalized_factor, treatments, genotypes) {
    # Generate an empty data table
    DT.list <- generateNorms(
      readin_summary_dt_final,
      normalized_factor,
      groups,
      geno = NULL,
      apply_genotype_filter = FALSE,
      treatments = treatments,
      genotypes = genotypes
    )
    
    # Combine data tables
    norm_keep <- do.call(cbind, c(
      DT.list[1],
      lapply(
        DT.list[2:length(DT.list)],
        function(x) x[, .SD, .SDcols = ncol(x)]
      )
    ))

    # Identify columns after the 9th column
    cols_after_ninth <- names(norm_keep)[10:ncol(norm_keep)]
    
    # Filter columns that include "norm" in their name
    norm_cols <- grep("norm", cols_after_ninth, value = TRUE)

    # Create the final list of columns to keep, including the first 9 columns
    final_cols <- c(names(norm_keep)[1:9], norm_cols)
    
    # Subset the data table to keep only the selected columns
    norm_keep <- norm_keep[, ..final_cols]
    
    return(norm_keep)
  }

  # compute statistics function
  summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE, conf.interval=.95, .drop = TRUE) {
    # Handle NA's in length
    length2 <- function (x, na.rm=FALSE) {
      if (na.rm) sum(!is.na(x)) else length(x)
    }
    
    # Calculate summary statistics
    datac <- ddply(data, groupvars, .drop=.drop, .fun = function(xx, col) {
      c(N = length2(xx[[col]], na.rm=na.rm),
        mean = mean(xx[[col]], na.rm=na.rm),
        sd = sd(xx[[col]], na.rm=na.rm))
    }, measurevar)
    
    # Rename columns
    datac <- rename(datac, c("mean" = paste(measurevar, "mean",sep = "_")))
    datac$se <- datac$sd / sqrt(datac$N)  # Standard error
    ciMult <- qt(conf.interval/2 + .5, datac$N-1)  # Confidence interval multiplier
    datac$ci <- datac$se * ciMult
    
    # Rename remaining columns
    datac <- rename(datac, c("sd" = paste(measurevar, "sd",sep = "_")))
    datac <- rename(datac, c("se" = paste(measurevar, "se",sep = "_")))
    datac <- rename(datac, c("ci" = paste(measurevar, "ci",sep = "_")))
    
    return(datac)
  }

  statsSummary <- function(readin_summary_dt_final, groups, normalized_factor, treatments, genotypes) {
    # Subset the data based on selected treatments and genotypes
    if (length(treatments) > 0) {
      readin_summary_dt_final <- readin_summary_dt_final[treatment %in% treatments]
    }
      
    if (length(genotypes) > 0) {
      readin_summary_dt_final <- readin_summary_dt_final[genotype %in% genotypes]
    }
      
    # Generate summary statistics for normalized groups
    stat_summary <- generateSE(readin_summary_dt_final, groups, norm = FALSE)
      
    return(stat_summary)
  }

  # observation for creating summary table
  # and sleep population plot.
  observe({

    req(input$meta_file)
    metadata <- read.csv(input$meta_file$datapath)
    metadata <- na.omit(metadata)
    summary_dt_final <- data.table()

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
    metadata_proc(metadata_proc)
    output$contents <- DT::renderDataTable(
      metadata,
      filter = list(position = "top", clear = FALSE, plain = TRUE)
    )

    observeEvent(input$save_process, {
      withBusyIndicatorServer("save_process", {
        
        req(input$batch_title, input$num_days)
        message(paste("Calculating... Please Wait."))
        batch_title <- input$batch_title
        batch_title(input$batch_title)
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
        dt_curated_final(dt_curated_final)

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

          # update the reactive val, outside of this observe bracket, for use in subsequent analysis.
          summary_dt_final(summary_dt_final)

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
    
    # Plot Data Tab:
    # update genotype, and treatment choices
    
  })

  # observation for selection of groups
  # this will require the summary_dt_final, and batch_title,
  # these two variables will HAVE to be reactive.
    observe({
    zt_bins_selected <- input$zt_bins
    batch_title <- batch_title()
    
    # Convert additional_choices to a named vector if needed
    additional_choices_named <- setNames(
      paste0("sleep_time_", zt_bins_selected),  # Actual values with prefix
      zt_bins_selected  # Labels to display
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

    meta_data_temp <- metadata_proc()

    # Update choices for 'treatments'
    updateCheckboxGroupInput(
      session,
      "treatments",
      choices = unique(meta_data_temp$treatment)
    )

    # Update choices for 'genos'
    updateCheckboxGroupInput(
      session,
      "genotypes",
      choices = unique(meta_data_temp$genotype)
    )

    updateSelectInput(
      session,
      "geno_plot",
      choices = unique(meta_data_temp$genotype)
    )

    updateCheckboxGroupInput(
      session,
      "treat_plot",
      choices = unique(meta_data_temp$treatment)
    )
  })

  # Define a reactive value to store normalized summary, which will be used
  # in download button, AND norm_stat_summary.
  norm_sum <- reactiveVal()

  # Generate normalized summary table
  observeEvent(input$norm_summary, {
    req(input$groups)
    req(input$norm_summary)
    req(summary_dt_final())
    req(input$treatments)
    req(input$genotypes)

    groups <- input$groups
    treatments <- input$treatments
    genotypes <- input$genotypes
    
    message("Normalizing selected groups...")
    # Retrieve the data table from reactive value
    summary_dt_final_data <- summary_dt_final()
    norm_factor <- summary_dt_final_data[, lapply(.SD, mean), by = .(genotype, treatment), .SDcols = groups]

    # update reactive norm_factor
    norm_factor(norm_factor)
    
    # Generate normalized summary
    norm_summary <- normSummary(summary_dt_final_data, groups, norm_factor, treatments, genotypes)
    norm_sum(norm_summary)  # Update reactive value

    message("Normalization complete!")
  })

  # Download normalized summary as CSV
  observeEvent(input$down_norm_sum, {
    req(norm_sum())  # Ensure results are available
    req(input$down_norm_sum)  # Ensure button is clicked
    batch_title <- input$batch_title
    
    # Write CSV file
    file_path <- paste0("generated_files/norm_summary_", batch_title, ".csv")
    write.csv(norm_sum(), file = file_path, row.names = FALSE)
    
    showModal(modalDialog(
      title = "Download Complete",
      paste0("Downloaded as ", file_path),
      easyClose = TRUE,
      footer = NULL
    ))
  })

  # reactive value for download
  norm_stat_sum <- reactiveVal()

  # generate statistics for normalized groups
  observeEvent(input$norm_stat_summary, {
    req(input$norm_stat_summary) # Ensure button is clicked
    req(norm_sum()) # Ensure initial norm_summary has been generated
    req(input$groups) # Ensure groups are selected
    req(input$treatments) # Ensure treatments are selected
    req(input$genotypes) # Ensure genotypes are selected

    message("generating statistics for normalized groups...")
    
    # retrieve values.
    groups <- input$groups
    treatments <- input$treatments
    genotypes <- input$genotypes
    summary_dt_final_data <- summary_dt_final()
    norm_summary <- norm_sum()

    stat_summary <- generateSE(norm_summary, groups, norm = TRUE)

    #update reacive value for download.
    norm_stat_sum(stat_summary)

    message("Normalization complete!")
  })

  observeEvent(input$down_norm_stat, {
    # allow user to download csv for statistics generated for normalized values.
    req(norm_stat_sum()) # ensure statistics have been generated
    req(input$down_norm_stat) # ensure button is clicked
    req(input$batch_title) # ensure batch title is available

    batch_title <- input$batch_title

    file_path <- paste0("generated_files/norm_stat_summary_", batch_title, ".csv")
    write.csv(norm_stat_sum(), file = file_path, row.names = FALSE)
    
    showModal(modalDialog(
      title = "Download Complete",
      paste0("Downloaded as ", file_path),
      easyClose = TRUE,
      footer = NULL
    ))

  })

  stat_sum <- reactiveVal()
  # generate statistics for groups

  observeEvent(input$stat_summary, {
    req(input$stat_summary)  # Ensure button is clicked
    req(summary_dt_final()) # Ensure data is available
    req(input$groups)       # Ensure groups are selected
    req(input$treatments)   # Ensure treatments are selected
    req(input$genotypes)    # Ensure genotypes are selected
    req(norm_factor())      # Ensure normalization factors are available

    message("Generating Statistics...")

    groups <- input$groups
    treatments <- input$treatments
    genotypes <- input$genotypes
    summary_dt_final_data <- summary_dt_final()
    

    stat_summary <- statsSummary(summary_dt_final_data, groups, norm_factor(), treatments, genotypes)
    stat_sum(stat_summary)

    message("Statistics generated!")
  })

  observeEvent(input$down_stat_sum, {
    req(input$down_stat_sum)
    req(input$batch_title)
    req(stat_sum())

    batch_title <- input$batch_title
    
    file_path <- paste0("generated_files/stat_summary_", batch_title, ".csv")
    write.csv(stat_sum(), file = file_path, row.names = FALSE)
    
    showModal(modalDialog(
      title = "Download Complete",
      paste0("Downloaded as ", file_path),
      easyClose = TRUE,
      footer = NULL
    ))

  })

  reactiveSleepWrap <- reactive({
    req(input$geno_plot)
    req(input$treat_plot)
    req(input$norm_plot)

    dt_curated_final <- dt_curated_final()

    # Filter by genotype
    filtered_data <- dt_curated_final[xmv(genotype) == input$geno_plot]
    
    # Filter by multiple treatments
    if (length(input$treat_plot) > 0) {
      filtered_data <- filtered_data[xmv(treatment) %in% input$treat_plot]
    }

    filtered_data
  })

  # Reactive expression for treatment colors
  treatmentColors <- reactive({
    req(input$geno_plot)
    req(input$treat_plot)
    req(input$norm_plot)
    
    treatments <- input$treat_plot
    
    if (is.null(treatments) || length(treatments) == 0) {
      return(c())  # Return an empty vector if no treatments are selected
    }
    
    num_colors <- length(treatments)
    
    # Combine colors from multiple palettes
    palette1 <- RColorBrewer::brewer.pal(9, "Set1")
    palette2 <- RColorBrewer::brewer.pal(8, "Set2")
    colors <- c(palette1, palette2)[1:num_colors]
    
    # Create a named vector of colors
    color_vector <- setNames(colors, treatments)
    color_vector
  })

  # Render plot output
  output$pop_sleep_wrap <- renderPlot({
    req(input$save_process)
    req(input$geno_plot)
    req(input$treat_plot)
    req(input$norm_plot)

    batch_title <- input$batch_title
    filtered_data <- reactiveSleepWrap()
    color_vector <- treatmentColors()

    ggetho(filtered_data,
          aes(y = asleep * 100, colour = treatment), time_wrap = hours(24)) +
      stat_pop_etho() +
      scale_y_continuous(name = "Sleep (%)", limits = c(0, 100), expand = c(0, 0)) +
      scale_colour_manual(values = color_vector, breaks = names(color_vector),
                          labels = names(color_vector),
                          aesthetics = c("colour", "fill")) +
      # stat_ld_annotations() +
      facet_grid(genotype ~ .) +
      ggtitle(paste0(batch_title, "_", input$geno_plot)) +
      theme(
        title = element_text(size = 6, face = "bold"),
        axis.title.y = element_blank(),
        legend.position = "top"
      )
  })
}
