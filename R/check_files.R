#' Check DeepLabCut CSVs for likelihood quality (robust version with summary folder)
#'
#' This function scans DeepLabCut CSVs (with 3 header rows) and calculates
#' how many frames have both snout and base-of-tail likelihoods above 0.6.
#' It saves a summary table in a new subfolder called "LikelihoodSummary".
#'
#' @param folder Path to the folder containing DeepLabCut CSVs (e.g. "Relative CSVs").
#' @return A data frame summarizing each file, also saved as "Likelihood_summary.csv"
#'         inside a "LikelihoodSummary" subfolder.
#' @export
check_files <- function(folder) {
  library(dplyr)

  message("Checking DeepLabCut likelihoods in: ", folder)
  if (!dir.exists(folder)) stop("Folder not found: ", folder)

  # Prepare output folder
  summary_folder <- file.path(folder, "LikelihoodSummary")
  if (!dir.exists(summary_folder)) {
    dir.create(summary_folder)
    message("Created summary folder: ", summary_folder)
  }

  # Find input CSVs (excluding previous outputs)
  csv_files <- list.files(folder, pattern = "\\.csv$", full.names = TRUE)
  csv_files <- csv_files[!grepl("Likelihood_summary", csv_files, ignore.case = TRUE)]

  if (length(csv_files) == 0) stop("No suitable CSV files found in folder: ", folder)

  # Container for results
  results <- data.frame(
    File = character(),
    Frames_Above_0.6 = numeric(),
    Total_Frames = numeric(),
    Percentage = numeric(),
    stringsAsFactors = FALSE
  )

  for (file_path in csv_files) {
    # Skip empty or malformed files
    if (file.size(file_path) == 0) {
      warning("Skipping empty file: ", basename(file_path))
      next
    }

    header <- try(read.csv(file_path, nrows = 3, header = FALSE), silent = TRUE)
    if (inherits(header, "try-error") || nrow(header) < 3) {
      warning("Skipping non-DLC or malformed file: ", basename(file_path))
      next
    }

    data <- try(read.csv(file_path, skip = 3, header = FALSE), silent = TRUE)
    if (inherits(data, "try-error") || nrow(data) == 0) {
      warning("Skipping unreadable file: ", basename(file_path))
      next
    }

    # Merge 3-line header into single names
    colnames(data) <- apply(header, 2, function(x) paste(na.omit(x), collapse = "_"))

    # Find relevant columns
    snout_lh_col <- grep("snout[._ -]*tip[._ -]*likelihood", names(data), value = TRUE, ignore.case = TRUE)
    base_lh_col  <- grep("base[._ -]*of[._ -]*tail[._ -]*likelihood", names(data), value = TRUE, ignore.case = TRUE)

    if (length(snout_lh_col) > 0 && length(base_lh_col) > 0) {
      above_0.6 <- data %>%
        filter(.data[[snout_lh_col]] > 0.6 & .data[[base_lh_col]] > 0.6)

      total_frames <- nrow(data)
      good_frames  <- nrow(above_0.6)
      percent <- round((good_frames / total_frames) * 100, 2)

      results <- rbind(
        results,
        data.frame(
          File = basename(file_path),
          Frames_Above_0.6 = good_frames,
          Total_Frames = total_frames,
          Percentage = percent,
          stringsAsFactors = FALSE
        )
      )
    } else {
      warning("Missing likelihood columns in ", basename(file_path))
    }
  }

  # Save summary
  output_file <- file.path(summary_folder, "Likelihood_summary.csv")
  write.csv(results, output_file, row.names = FALSE)

  message("\n Summary saved as 'Likelihood_summary.csv' in: ", summary_folder)
  return(results)
}
