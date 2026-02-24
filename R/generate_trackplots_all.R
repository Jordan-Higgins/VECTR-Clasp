#' Generate animated snout trackplots for all CSVs in a folder
#'
#' This function loops through all "Relative CSVs" in a folder and generates
#' animated trajectory plots (.gif) showing the snout tip movement relative
#' to the base of the tail. Each animation is saved in a "Trackplots" subfolder.
#'
#' @param folder Path to the folder containing "Relative CSVs".
#' @param fps Frames per second for the animation (default = 30).
#' @param width Output GIF width in pixels (default = 800).
#' @param height Output GIF height in pixels (default = 600).
#' @return A vector of paths to the generated GIF files.
#' @export
generate_trackplots_all <- function(folder, fps = 30, width = 800, height = 600) {
  # Check required packages
  if (!requireNamespace("gganimate", quietly = TRUE) ||
      !requireNamespace("viridis", quietly = TRUE) ||
      !requireNamespace("gifski", quietly = TRUE) ||
      !requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Please install the required packages: gganimate, viridis, gifski, ggplot2")
  }

  library(ggplot2)
  library(gganimate)
  library(viridis)

  if (!dir.exists(folder)) stop("Folder not found: ", folder)

  # Output folder for GIFs
  output_folder <- file.path(folder, "Trackplots")
  if (!dir.exists(output_folder)) {
    dir.create(output_folder)
    message("Created output folder: ", output_folder)
  }

  # Find CSVs
  csv_files <- list.files(folder, pattern = "\\.csv$", full.names = TRUE)
  if (length(csv_files) == 0) stop("No CSV files found in folder: ", folder)

  message("Generating trackplots for ", length(csv_files), " files...")

  gif_paths <- c()

  for (file_path in csv_files) {
    message("\nProcessing: ", basename(file_path))

    data <- try(read.csv(file_path), silent = TRUE)
    if (inherits(data, "try-error") || nrow(data) == 0) {
      warning("Skipping unreadable or empty file: ", basename(file_path))
      next
    }

    if (!all(c("snout_tip_rel_x", "snout_tip_rel_y") %in% names(data))) {
      warning("Missing snout coordinate columns in ", basename(file_path))
      next
    }

    data$Frame <- seq_len(nrow(data))

    p <- ggplot(data, aes(x = snout_tip_rel_x, y = snout_tip_rel_y)) +
      geom_path(alpha = 0.3, color = "grey70") +
      geom_point(aes(color = Frame), size = 2) +
      geom_point(aes(x = 0, y = 0), color = "black", size = 3) +
      geom_text(aes(x = 0, y = 0, label = "Base of tail"),
                vjust = -1, hjust = 0.5, color = "black", size = 3) +
      scale_color_viridis_c() +
      theme_minimal(base_size = 14) +
      labs(
        title = paste0("Relative Snout Tip Trajectory - ", basename(file_path)),
        subtitle = "Frame: {frame}",
        x = "Relative X Position (pixels)",
        y = "Relative Y Position (pixels, inverted)",
        color = "Frame"
      ) +
      coord_equal() +
      scale_y_reverse()

    anim <- p +
      transition_reveal(along = Frame) +
      ease_aes('linear')

    gif_path <- file.path(output_folder, paste0(tools::file_path_sans_ext(basename(file_path)), "_trackplot.gif"))

    message("Rendering animation...")
    gganimate::animate(
      anim,
      fps = fps,
      duration = nrow(data) / fps,
      width = width,
      height = height,
      renderer = gifski_renderer(loop = TRUE)
    )

    gganimate::anim_save(gif_path, animation = last_animation())
    gif_paths <- c(gif_paths, gif_path)
    message("Saved: ", gif_path)
  }

  message("\nAll trackplots saved in: ", output_folder)
  return(gif_paths)
}
