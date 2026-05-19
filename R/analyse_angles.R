#' Analyse DeepLabCut angle CSVs
#'
#' This function analyses the angular data produced by the DeepLabCut processing
#' pipeline. It computes circular statistics, left/right preference, and swing
#' counts, and saves the results in a subfolder called "Analysis".
#'
#' @param folder Path to the folder containing the "Angle CSVs" (e.g. "Filtered CSVs/Angle CSVs").
#' @param mid_angle The midline reference in degrees (default = 90, for upright orientation).
#' @param deadband Degrees around the midline considered "center" (default = 45).
#' @return A data frame summarizing each file. Also saves "SnoutAngleSummary_[deadband]deg.csv"
#'         inside a subfolder named "Analysis".
#' @export
analyse_angles <- function(folder, mid_angle = 90, deadband = 45) {
  library(dplyr)
  library(circular)

  message("Analysing angle data in: ", folder)

  if (!dir.exists(folder)) stop("Folder not found: ", folder)

  # Prepare output folder
  analysis_folder <- file.path(folder, "Analysis")
  if (!dir.exists(analysis_folder)) {
    dir.create(analysis_folder)
    message("Created output folder: ", analysis_folder)
  }

  # List CSVs
  csv_files <- list.files(folder, pattern = "\\.csv$", full.names = TRUE)
  csv_files <- csv_files[!grepl("SnoutAngleSummary", csv_files, ignore.case = TRUE)]

  if (length(csv_files) == 0) stop("No suitable CSV files found in folder: ", folder)

  # Summary results container
  summary_results <- data.frame(
    File = character(),
    Mean_Angle_Deg = numeric(),
    Mean_Resultant_Length = numeric(),
    Circular_SD = numeric(),
    Prop_Right = numeric(),
    Prop_Left = numeric(),
    Swing_Count = numeric(),
    stringsAsFactors = FALSE
  )

  # Loop over files
  for (file_path in csv_files) {
    message("Processing: ", basename(file_path))

    data <- try(read.csv(file_path), silent = TRUE)
    if (inherits(data, "try-error") || nrow(data) == 0) {
      warning("Skipping unreadable file: ", basename(file_path))
      next
    }

    if (all(c("angle_rad", "angle_deg") %in% names(data))) {
      data$angle_deg <- (data$angle_deg + 360) %% 360

      # Convert to circular
      angles <- circular(data$angle_rad, units = "radians", template = "none", modulo = "2pi")

      mean_angle <- mean.circular(angles)
      mean_resultant_length <- rho.circular(angles)
      circ_sd <- sd.circular(angles)
      mean_angle_deg <- (as.numeric(mean_angle) * 180 / pi) %% 360

      # Left/right proportions
      prop_right <- mean(data$angle_deg < (mid_angle - deadband), na.rm = TRUE)
      prop_left  <- mean(data$angle_deg > (mid_angle + deadband), na.rm = TRUE)

      # Swing classification
      data$swing_state <- dplyr::case_when(
        data$angle_deg < (mid_angle - deadband) ~ "Right",
        data$angle_deg > (mid_angle + deadband) ~ "Left",
        TRUE ~ "Center"
      )

      # Collapse short jitters and count L↔R transitions
      swing_seq <- rle(data$swing_state)$values
      swing_count <- sum(swing_seq %in% c("Left", "Right")) #left or right exit out of deadband counts as swing

      # Append results
      summary_results <- rbind(
        summary_results,
        data.frame(
          File = basename(file_path),
          Mean_Angle_Deg = round(mean_angle_deg, 2),
          Mean_Resultant_Length = round(mean_resultant_length, 3),
          Circular_SD = round(circ_sd, 3),
          Prop_Right = round(prop_right, 3),
          Prop_Left = round(prop_left, 3),
          Swing_Count = swing_count,
          stringsAsFactors = FALSE
        )
      )
    } else {
      warning("Missing angle columns in ", basename(file_path))
    }
  }

  # Save summary
  summary_filename <- paste0("SnoutAngleSummary_", deadband, "deg.csv")
  summary_file <- file.path(analysis_folder, summary_filename)
  write.csv(summary_results, summary_file, row.names = FALSE)

  message("\n Snout angle summary saved to: ", summary_file)
  return(summary_results)
}
