# =============================================================================
# Plotting Utilities for Spatial Data Visualization
# =============================================================================
# This file contains functions for creating various visualizations of
# cell data and spatial geometries using ggplot2
# =============================================================================

library(tidyverse)

# Color palette for exclusion categories using University of Chicago colors
# Selected colors for different exclusion states (not, outside, inside, etc.)
exclude_colors = ggsci::pal_uchicago()(9)[c(2,3,6,5)]
# Colors: grey (#767676FF), light blue (#5B9BD5FF), orange (#FF9F1AFF), red (#C5050CFF)

# =============================================================================
# COMPARISON PLOTS
# =============================================================================

#' Create comprehensive comparison plots between Cortana and Halo data
#' 
#' Generates three visualization levels: side-by-side overview, medium zoom overlay,
#' and high zoom overlay. Saves plots to output/plots/ directory and returns plot objects.
#' 
#' @param dc Cortana cell data with position columns and exclusion info
#' @param dh Halo rectangle data with geometry and exclusion info
#' @return List of three ggplot objects (overview, medium zoom, high zoom)
#' @export
plot_comparisons <- function(dc,dh) {

  sample_id=dc$sample_id[1]
  fov=dc$FOV[1]

  # Create side-by-side comparison plots
  ph=plot_rectangles(dh) + labs(subtitle="Halo",title=paste(sample_id,"FOV:",fov))
  pc=plot_cells_basic(dc) + labs(subtitle="Cortana",title=paste(sample_id,"FOV:",fov))

  p1=ph + pc

  # Calculate center point for zoomed views
  Xcm=dh %>% pull(X) %>% mean
  Ycm=dh %>% pull(Y) %>% mean

  # Create overlay plots at different zoom levels
  # Medium zoom (50% of full view)
  s=.5;
  p2=plot_cells_and_rectangles(dh,dc,xlim=s*16*c(-100,100)+Xcm,ylim=s*9*c(-100,100)+Ycm)
  # High zoom (20% of full view) 
  s=.2;
  p3=plot_cells_and_rectangles(dh,dc,xlim=s*16*c(-100,100)+Xcm,ylim=s*9*c(-100,100)+Ycm)

  # Save plots to PDF file
  fs::dir_create("output/plots")
  pdf(file=cc("output/plots/pltsHaloVsCortana",sample_id,fov,".pdf"),width=11,height=8.5)
  print(ph+pc)  # Side-by-side comparison
  print(p2)     # Medium zoom overlay
  print(p3)     # High zoom overlay
  dev.off()

  # Return plot objects for potential further use
  list(p1,p2,p3)

}

# =============================================================================
# BASIC PLOTTING FUNCTIONS
# =============================================================================

#' Create basic scatter plot of cell positions with exclusion color coding
#' 
#' Plots cell positions as points colored by exclusion category (ExcDesc).
#' Uses University of Chicago color palette for consistent exclusion visualization.
#' 
#' @param data Cell data with position columns and ExcDesc exclusion categories
#' @param x_col Name of X coordinate column (default "Xc")
#' @param y_col Name of Y coordinate column (default "Yc") 
#' @param alpha Point transparency (default 0.25)
#' @return ggplot object with color-coded cells
#' @export
plot_cells_basic <- function(data, x_col = "Xc", y_col = "Yc", alpha = 0.25) {
  ggplot(data, aes(.data[[x_col]], .data[[y_col]],color=ExcDesc)) +
    geom_point(alpha = alpha) + 
    theme_minimal() +
    scale_y_reverse() +  # Invert Y-axis to match image coordinates
    scale_color_manual(values=exclude_colors) +  # Apply exclusion color scheme
    coord_fixed()  # Equal aspect ratio for spatial data
}

#' Plot rectangular regions from geometry data with exclusion color coding
#' 
#' Creates hollow rectangles colored by exclusion category. Useful for visualizing
#' Halo segmentation regions with their exclusion status.
#' 
#' @param rect_data Data with XMin, XMax, YMin, YMax columns and Exclude categories
#' @param xlim X-axis limits for zooming (optional)
#' @param ylim Y-axis limits for zooming (optional)
#' @return ggplot object showing color-coded hollow rectangles
#' @export
plot_rectangles <- function(rect_data, xlim = NULL, ylim = NULL) {
  p <- ggplot() + 
    theme_minimal() + 
    geom_rect(data = rect_data, 
              aes(xmin = XMin, xmax = XMax, ymin = YMin, ymax = YMax, color=Exclude),
              fill = NA) +  # Hollow rectangles (no fill, only borders)
    coord_fixed() +  # Maintain aspect ratio
    scale_y_reverse() +  # Invert Y-axis to match image coordinates
    scale_color_manual(values=exclude_colors)  # Apply exclusion color scheme
  
  # Apply zoom limits if provided
  if (!is.null(xlim) && !is.null(ylim)) {
    p <- p + coord_cartesian(xlim = xlim, ylim = ylim)
  }
  
  p
}

#' Plot overlay of rectangular regions and cell points with exclusion colors
#' 
#' Creates a combined visualization showing Halo rectangles (background) with 
#' Cortana cell points (foreground). Rectangles are colored by exclusion status,
#' while cell points use default styling. Useful for spatial comparison analysis.
#' 
#' @param rect_data Rectangle data with XMin/XMax/YMin/YMax and exclusion info
#' @param cell_data Cell data with position columns
#' @param xlim X-axis limits for zooming (optional)
#' @param ylim Y-axis limits for zooming (optional) 
#' @param x_col Cell X coordinate column name (default "Xc")
#' @param y_col Cell Y coordinate column name (default "Yc")
#' @return Combined ggplot showing color-coded rectangles with overlaid cell points
#' @export
plot_cells_and_rectangles <- function(rect_data, cell_data, 
                                     xlim = NULL, ylim = NULL,
                                     x_col = "Xc", y_col = "Yc") {
  p <- ggplot() + 
    theme_minimal() + 
    # Draw rectangles first (background layer)
    geom_rect(data = rect_data, 
              aes(xmin = XMin, xmax = XMax, ymin = YMin, ymax = YMax, color=Exclude),
              fill = NA) +  # Hollow rectangles
    # Add cell points on top (foreground layer)
    geom_point(data = cell_data, aes(.data[[x_col]], .data[[y_col]])) +
    scale_color_manual(values=exclude_colors[1:2])  # Use subset of colors for rectangles

  # Apply zoom limits and coordinate system
  if (!is.null(xlim) && !is.null(ylim)) {
    p <- p + coord_fixed(xlim = xlim, ylim = ylim)  # Zoomed view with fixed aspect ratio
  } else {
    p <- p + coord_fixed()  # Full view with fixed aspect ratio
  }
  
  p + scale_y_reverse()  # Invert Y-axis to match image coordinates
}