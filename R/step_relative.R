#' @noRd
step_relative <- function(input_folder) {
  library(dplyr)

  output_folder <- file.path(dirname(input_folder), "Relative CSVs")
  if (!dir.exists(output_folder)) dir.create(output_folder)

  csv_files <- list.files(input_folder, pattern = "\\.csv$", full.names = TRUE)

  for (file_path in csv_files) {
    try({
      data <- read.csv(file_path)

      snout_x_col <- grep("snout.tip_x", names(data), value = TRUE)
      snout_y_col <- grep("snout.tip_y", names(data), value = TRUE)
      base_x_col  <- grep("base.of.tail_x", names(data), value = TRUE)
      base_y_col  <- grep("base.of.tail_y", names(data), value = TRUE)

      data <- data %>%
        mutate(
          snout_tip_rel_x = .data[[snout_x_col]] - .data[[base_x_col]],
          snout_tip_rel_y = .data[[snout_y_col]] - .data[[base_y_col]]
        )

      output_file <- file.path(
        output_folder,
        sub("\\.csv$", "_relative.csv", basename(file_path))
      )
      write.csv(data, output_file, row.names = FALSE)
    }, silent = TRUE)
  }

  return(output_folder)
}
