#' @noRd
step_angle <- function(input_folder) {
  library(dplyr)

  output_folder <- file.path(dirname(input_folder), "Angle CSVs")
  if (!dir.exists(output_folder)) dir.create(output_folder)

  csv_files <- list.files(input_folder, pattern = "\\.csv$", full.names = TRUE)

  for (file_path in csv_files) {
    data <- read.csv(file_path)
    if (all(c("snout_tip_rel_x", "snout_tip_rel_y") %in% names(data))) {
      data <- data %>%
        mutate(
          angle_rad = atan2(snout_tip_rel_y, snout_tip_rel_x),
          angle_deg = (angle_rad * 180 / pi) %% 360,
          radius = sqrt(snout_tip_rel_x^2 + snout_tip_rel_y^2)
        )

      output_file <- file.path(
        output_folder,
        sub("\\.csv$", "_angle.csv", basename(file_path))
      )
      write.csv(data, output_file, row.names = FALSE)
    }
  }

  return(output_folder)
}
