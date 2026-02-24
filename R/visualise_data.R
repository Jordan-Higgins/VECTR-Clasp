#' Visualise DeepLabCut data (trackplots, heatmaps, and polar histograms)
#'
#' Creates high-quality visualisations:
#' - Trackplots of snout trajectories
#' - 2D kernel density heatmaps
#' - Polar histograms of angles
#'
#' All figures include labeled axes and legends, and are saved as 600 dpi TIFFs
#' inside a "Visualisation" folder.
#'
#' @param folder Path to the parent folder containing "Filtered CSVs" with subfolders:
#'        "Relative CSVs" and/or "Angle CSVs".
#' @export
visualise_data <- function(folder) {
  library(ggplot2)
  library(viridis)
  library(MASS)
  library(dplyr)
  library(tools)

  message("Visualising data in: ", folder)

  # --- Output folder ---
  output_folder <- file.path(folder, "Visualisation")
  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
    message("Created folder: ", output_folder)
  }

  # Trackplots
  rel_folder <- file.path(folder, "Relative CSVs")
  if (dir.exists(rel_folder)) {
    message("\nGenerating trackplots...")

    csv_files <- list.files(rel_folder, pattern = "\\.csv$", full.names = TRUE)
    for (file_path in csv_files) {
      message("  Processing: ", basename(file_path))
      data <- try(read.csv(file_path), silent = TRUE)
      if (inherits(data, "try-error") || nrow(data) == 0) next
      if (!all(c("snout_tip_rel_x", "snout_tip_rel_y") %in% names(data))) next

      data$Frame <- seq_len(nrow(data))
      xlim <- range(data$snout_tip_rel_x, na.rm = TRUE)
      ylim <- range(data$snout_tip_rel_y, na.rm = TRUE)

      p <- ggplot(data, aes(x = snout_tip_rel_x, y = snout_tip_rel_y, color = Frame)) +
        geom_path(linewidth = 0.8, lineend = "round") +
        scale_color_viridis_c(option = "plasma", name = "Frame") +
        annotate("point", x = 0, y = 0, color = "black", size = 2.5) +
        coord_equal(expand = TRUE, xlim = xlim, ylim = ylim) +
        scale_y_reverse() +
        labs(
          x = "Relative X position (pixels)",
          y = "Relative Y position (pixels, inverted)",
          title = paste0("Trajectory: ", basename(file_path))
        ) +
        theme_minimal(base_family = "Helvetica", base_size = 13) +
        theme(
          panel.grid = element_blank(),
          legend.position = "right",
          plot.margin = margin(10, 10, 10, 10)
        )

      out_file <- file.path(output_folder, paste0(tools::file_path_sans_ext(basename(file_path)), "_Trackplot.tiff"))
      ggsave(out_file, plot = p, device = "tiff", width = 8, height = 8,
             units = "in", dpi = 600, compression = "lzw")
    }
    message("Trackplots saved in: ", output_folder)
  }

  # Heatmaps
  if (dir.exists(rel_folder)) {
    message("\nGenerating heatmaps...")

    csv_files <- list.files(rel_folder, pattern = "\\.csv$", full.names = TRUE)
    for (file_path in csv_files) {
      message("  Processing: ", basename(file_path))
      data <- try(read.csv(file_path), silent = TRUE)
      if (inherits(data, "try-error") || nrow(data) == 0) next
      if (!all(c("snout_tip_rel_x", "snout_tip_rel_y") %in% names(data))) next

      d <- kde2d(data$snout_tip_rel_x, data$snout_tip_rel_y, n = 300)
      local_max <- max(d$z, na.rm = TRUE)
      xlim <- range(data$snout_tip_rel_x, na.rm = TRUE)
      ylim <- range(data$snout_tip_rel_y, na.rm = TRUE)

      p <- ggplot(data, aes(x = snout_tip_rel_x, y = snout_tip_rel_y)) +
        stat_density_2d(aes(fill = after_stat(density)), geom = "raster", contour = FALSE, n = 300) +
        scale_fill_viridis_c(option = "plasma", name = "Density",
                             limits = c(0, local_max * 0.9), oob = scales::squish) +
        coord_equal(expand = TRUE, xlim = xlim, ylim = ylim) +
        scale_y_reverse() +
        labs(
          x = "Relative X position (pixels)",
          y = "Relative Y position (pixels, inverted)",
          title = paste0("Density heatmap: ", basename(file_path))
        ) +
        theme_minimal(base_family = "Helvetica", base_size = 13) +
        theme(
          legend.position = "right",
          panel.grid = element_blank(),
          plot.margin = margin(10, 10, 10, 10)
        )

      out_file <- file.path(output_folder, paste0(tools::file_path_sans_ext(basename(file_path)), "_Heatmap.tiff"))
      ggsave(out_file, plot = p, device = "tiff", width = 8, height = 8,
             units = "in", dpi = 600, compression = "lzw")
    }
    message("Heatmaps saved in: ", output_folder)
  }

  # Polars
  angle_folder <- file.path(folder, "Angle CSVs")
  if (dir.exists(angle_folder)) {
    message("\nGenerating polar histograms...")

    csv_files <- list.files(angle_folder, pattern = "\\.csv$", full.names = TRUE)
    for (file_path in csv_files) {
      message("  Processing: ", basename(file_path))
      data <- try(read.csv(file_path), silent = TRUE)
      if (inherits(data, "try-error") || nrow(data) == 0) next
      if (!"angle_rad" %in% names(data)) next

      # Normalize and bin angles
      data$angle_rad <- (data$angle_rad %% (2 * pi))
      bin_edges <- seq(0, 2 * pi, by = pi / 18)
      hist_vals <- hist(data$angle_rad, breaks = bin_edges, plot = FALSE)
      df_hist <- data.frame(angle = hist_vals$mids, count = hist_vals$counts)
      ymax <- max(df_hist$count) * 1.1  # autoscale radial limit

      # Create polar histogram with visible degrees + radial axis
      p <- ggplot(df_hist, aes(x = angle, y = count)) +
        geom_bar(
          stat = "identity", width = pi / 18,
          fill = "#4E91F6", color = "white", alpha = 0.85
        ) +
        # Angular (degree) axis around the circle
        scale_x_continuous(
          limits = c(0, 2 * pi),
          breaks = seq(0, 2 * pi - pi/4, by = pi / 4),
          labels = c("0\u00b0", "45\u00b0", "90\u00b0", "135\u00b0", "180\u00b0", "225\u00b0", "270\u00b0", "315\u00b0")
        ) +
        # Radial axis with numeric tick marks
        scale_y_continuous(
          limits = c(0, ymax),
          breaks = seq(0, ymax, length.out = 5),
          labels = function(x) sprintf("%.0f", x),
          expand = c(0, 0)
        ) +
        coord_polar(start = pi / 2, direction = -1, clip = "off") +
        labs(
          title = paste0("Polar histogram: ", basename(file_path)),
          x = NULL,
          y = "Frequency"
        ) +
        theme_minimal(base_family = "Helvetica", base_size = 14) +
        theme(
          axis.text.x = element_text(size = 10, color = "black"),
          axis.text.y = element_text(size = 9, color = "black"),
          axis.ticks = element_blank(),
          panel.grid.major.y = element_line(color = "grey75", linewidth = 0.4),
          panel.grid.major.x = element_line(color = "grey85", linewidth = 0.3),
          plot.margin = margin(15, 15, 15, 15)
        )

      out_file <- file.path(output_folder, paste0(tools::file_path_sans_ext(basename(file_path)), "_PolarHistogram.tiff"))
      ggsave(out_file, plot = p, device = "tiff", width = 8, height = 8,
             units = "in", dpi = 600, compression = "lzw")
    }
    message("Polar histograms saved in: ", output_folder)
  } else {
    message("Skipping polar histograms, no 'Angle CSVs' folder found.")
  }

  message("\n All visualisations saved in: ", output_folder)
}
