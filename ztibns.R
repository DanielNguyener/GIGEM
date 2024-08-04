# testing zt bins function
library(shiny)
library(data.table)
library(damr)
library(ggetho)
library(DT)
library(sleepr)

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

metadata <- read.csv("loadinginfo_Batch240622.csv")
metadata_proc <- link_dam_metadata(metadata, result_dir = ".")
dt_curated_final <- dt_final

dt <- behavr(dt_curated_final, metadata_proc)

summary_dt_final <- rejoin(dt[, .(sleep_fraction = mean(asleep)), by = id])
dt[, phase := ifelse(t %% hours(24) < hours(12), "L", "D")]

summary_dt_final <- rejoin(dt[, .(
  sleep_fraction = mean(asleep),
  sleep_fraction_all = mean(asleep),
  sleep_time_all = 1440 * mean(asleep),
  sleep_fraction_l = mean(asleep[phase == "L"]),
  sleep_time_l = 720 * mean(asleep[phase == "L"]),
  sleep_fraction_d = mean(asleep[phase == "D"]),
  sleep_time_d = 720 * mean(asleep[phase == "D"])
), by = id])

zt_bins <- c("ZT_0_4","ZT_16_20","ZT_20_24")

save.image(file = "ztbins_test.RData")
load("ztbins_test.RData")
summary_dt_final <- ZTbins(dt, summary_dt_final, zt_bins)
