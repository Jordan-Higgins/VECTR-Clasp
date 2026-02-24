#' Analyse DeepLabCut data in a folder
#'
#' This function runs a three-step pipeline:
#' 1. Extracts snout and tail coordinates
#' 2. Computes relative positions
#' 3. Calculates angles and distances
#'
#' @param folder Path to the folder containing filtered DeepLabCut CSVs.
#' @return Creates subfolders and processed CSVs for each pipeline step.
#' @export
analyse_deeplabcut_data <- function(folder) {
  message("Starting DeepLabCut analysis pipeline in: ", folder)

  extracted <- step_extract(folder)
  relative  <- step_relative(extracted)
  angles    <- step_angle(relative)

  message("\n Analysis complete. Outputs saved in subdirectories of: ", folder)
  invisible(angles)
}
