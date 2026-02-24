#' Analyse DeepLabCut movement data
#'
#' This function analyses relative coordinate CSVs to calculate movement metrics
#' such as total travelled distance, net displacement, and mean per-frame movement.
#' All distances are scaled to centimetres based on a user-supplied calibration value.
#'
#' @param folder Path to the folder containing "Relative CSVs".
#' @param cm_per_10px Calibration factor: how many centimetres correspond to 10 pixels.
#' @return A data frame summarizing each file, also saved as "SnoutMovementSummary.csv"
#'         inside an "Analysis" subfolder.
#' @export
analyse_movement <- function(folder, cm_per_10px) {
  library(dplyr)

  message("Analysing movement data in: ", folder)
  if (!dir.exists(folder)) stop("Folder not found: ", folder)

  # --- Conversion factor ---
  px_to_cm <- cm_per_10px / 10  # convert per-pixel

  # --- Prepare output folder ---
  analysis_folder <- file.path(folder, "Analysis")
  if (!dir.exists(analysis_folder)) {
    dir.create(analysis_folder)
    message("Created analysis folder: ", analysis_folder)
  }

  # --- Find CSVs ---
  csv_files <- list.files(folder, pattern = "\\.csv$", full.names = TRUE)
  csv_files <- csv_files[!grepl("SnoutMovementSummary", csv_files, ignore.case = TRUE)]

  if (length(csv_files) == 0) stop("No suitable CSV files found in folder: ", folder)

  results <- data.frame(
    File = character(),
    Total_Distance_cm = numeric(),
    Net_Displacement_cm = numeric(),
    Mean_Frame_Movement_cm = numeric(),
    stringsAsFactors = FALSE
  )

  for (file_path in csv_files) {
    message("Processing: ", basename(file_path))

    data <- try(read.csv(file_path), silent = TRUE)
    if (inherits(data, "try-error") || nrow(data) == 0) {
      warning("Skipping unreadable file: ", basename(file_path))
      next
    }

    # Ensure required columns exist
    if (all(c("snout_tip_rel_x", "snout_tip_rel_y") %in% names(data))) {
      data <- data %>%
        mutate(
          dx = snout_tip_rel_x - lag(snout_tip_rel_x),
          dy = snout_tip_rel_y - lag(snout_tip_rel_y),
          step_distance_px = sqrt(dx^2 + dy^2)
        )

      # Compute movement metrics in pixels
      total_distance_px <- sum(data$step_distance_px, na.rm = TRUE)


      # Convert to cm
      total_distance_cm <- total_distance_px * px_to_cm


      # Append results
      results <- rbind(
        results,
        data.frame(
          File = basename(file_path),
          Total_Distance_cm = round(total_distance_cm, 2),
          stringsAsFactors = FALSE
        )
      )
    } else {
      warning("Missing snout coordinates in ", basename(file_path))
    }
  }

  # --- Save results ---
  output_file <- file.path(analysis_folder, "SnoutMovementSummary.csv")
  write.csv(results, output_file, row.names = FALSE)

  message("\n Snout movement summary saved to: ", output_file)
  return(results)
}
