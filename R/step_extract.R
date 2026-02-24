#' @noRd
step_extract <- function(input_folder) {
  library(dplyr)

  output_folder <- file.path(input_folder, "Extracted CSVs")
  if (!dir.exists(output_folder)) dir.create(output_folder)

  csv_files <- list.files(input_folder, pattern = "\\.csv$", full.names = TRUE)

  for (file_path in csv_files) {
    try({
      header <- read.csv(file_path, nrows = 3, header = FALSE)
      data <- read.csv(file_path, skip = 3, header = FALSE)
      colnames(data) <- apply(header, 2, function(x) paste(na.omit(x), collapse = "_"))

      selected_data <- data %>%
        select(
          contains("coords"),
          contains("snout tip_x"),
          contains("snout tip_y"),
          contains("snout tip_likelihood"),
          contains("base-of-tail_x"),
          contains("base-of-tail_y"),
          contains("base-of-tail_likelihood")
        )

      output_file <- file.path(
        output_folder,
        sub("\\.csv$", "_extracted_snout_and_base-of-tail.csv", basename(file_path))
      )
      write.csv(selected_data, output_file, row.names = FALSE)
    }, silent = TRUE)
  }

  return(output_folder)
}
