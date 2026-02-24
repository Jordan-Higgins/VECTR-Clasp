#' Generate animated snout track plot
#'
#' This function creates a dynamic trajectory plot showing the snout tip’s movement
#' relative to the base of the tail, using coordinates from a "Relative CSV".
#' The output is a .gif animation saved in a "Trackplots" folder within the same directory.
#'
#' @param file Path to a single "Relative CSV" file.
#' @param fps Frames per second for the animation (default = 30).
#' @param width Output GIF width in pixels (default = 800).
#' @param height Output GIF height in pixels (default = 600).
#' @return The path to the saved GIF file.
#' @export
generate_trackplot <- function(file, fps = 30, width = 800, height = 600) {
  # Load required libraries
  if (!requireNamespace("gganimate", quietly = TRUE) ||
      !requireNamespace("viridis", quietly = TRUE) ||
      !requireNamespace("gifski", quietly = TRUE) ||
      !requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Please install the required packages: gganimate, viridis, gifski, ggplot2")
  }

  library(ggplot2)
  library(gganimate)
  library(viridis)

  if (!file.exists(file)) stop("File not found: ", file)

  message("Generating trackplot for: ", basename(file))

  # Read the data
  data <- try(read.csv(file), silent = TRUE)
  if (inherits(data, "try-error") || nrow(data) == 0)
    stop("Could not read CSV or file is empty.")

  # Check columns
  if (!all(c("snout_tip_rel_x", "snout_tip_rel_y") %in% names(data))) {
    stop("Required columns (snout_tip_rel_x, snout_tip_rel_y) not found in file.")
  }

  # Add frame index
  data$Frame <- seq_len(nrow(data))

  # Output folder
  output_folder <- file.path(dirname(file), "Trackplots")
  if (!dir.exists(output_folder)) {
    dir.create(output_folder)
    message("Created output folder: ", output_folder)
  }

  # Base plot
  p <- ggplot(data, aes(x = snout_tip_rel_x, y = snout_tip_rel_y)) +
    geom_path(alpha = 0.3, color = "grey70") +
    geom_point(aes(color = Frame), size = 2) +
    geom_point(aes(x = 0, y = 0), color = "black", size = 3) +
    geom_text(aes(x = 0, y = 0, label = "Base of tail"),
              vjust = -1, hjust = 0.5, color = "black", size = 3) +
    scale_color_viridis_c() +
    theme_minimal(base_size = 14) +
    labs(
      title = "Relative Snout Tip Trajectory (Inverted Y-axis)",
      subtitle = "Frame: {frame}",
      x = "Relative X Position (pixels)",
      y = "Relative Y Position (pixels, inverted)",
      color = "Frame"
    ) +
    coord_equal() +
    scale_y_reverse()

  # Add animation
  anim <- p +
    transition_reveal(along = Frame) +
    ease_aes('linear')

  # Render
  message("Rendering animation...")
  gif_path <- file.path(output_folder, paste0(tools::file_path_sans_ext(basename(file)), "_trackplot.gif"))

  gganimate::animate(anim,
                     fps = fps,
                     duration = nrow(data) / fps,
                     width = width,
                     height = height,
                     renderer = gifski_renderer(loop = TRUE)
  )

  # Save animation
  gganimate::anim_save(gif_path, animation = last_animation())

  message("\n Trackplot saved as: ", gif_path)
  return(gif_path)
}
