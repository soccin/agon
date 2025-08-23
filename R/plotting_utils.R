# =============================================================================
# Plotting Utilities for Spatial Data Visualization
# =============================================================================
# This file contains functions for creating various visualizations of
# cell data and spatial geometries using ggplot2
# =============================================================================

library(tidyverse)

# =============================================================================
# COMPARISON PLOTS
# =============================================================================

plot_comparisons <- function(dc,dh) {

  sample_id=dc$sample_id[1]
  fov=dc$FOV[1]

  ph=plot_rectangles(dh) + labs(subtitle="Halo",title=paste(sample_id,"FOV:",fov))
  pc=plot_cells_basic(dc) + labs(subtitle="Cortana",title=paste(sample_id,"FOV:",fov))

  p1=ph + pc

  Xcm=dh %>% pull(X) %>% mean
  Ycm=dh %>% pull(Y) %>% mean

  s=.5;
  p2=plot_cells_and_rectangles(dh,dc,xlim=s*16*c(-100,100)+Xcm,ylim=s*9*c(-100,100)+Ycm)
  s=.2;
  p3=plot_cells_and_rectangles(dh,dc,xlim=s*16*c(-100,100)+Xcm,ylim=s*9*c(-100,100)+Ycm)

  fs::dir_create("plots")

  pdf(file=cc("plots/pltsHaloVsCortana",sample_id,fov,".pdf"),width=11,height=8.5)
  print(ph+pc)
  print(p2)
  print(p3)
  dev.off()

}

# =============================================================================
# BASIC PLOTTING FUNCTIONS
# =============================================================================

#' Create basic scatter plot of cell positions
#' 
#' @param data Cell data with position columns
#' @param x_col Name of X coordinate column (default "Xc")
#' @param y_col Name of Y coordinate column (default "Yc") 
#' @param alpha Point transparency (default 0.2)
#' @return ggplot object
#' @export
plot_cells_basic <- function(data, x_col = "Xc", y_col = "Yc", alpha = 0.25) {
  ggplot(data, aes(.data[[x_col]], .data[[y_col]])) + 
    geom_point(alpha = alpha) + 
    theme_minimal() +
    scale_y_reverse() +
    coord_fixed()  # Equal aspect ratio for spatial data
}

#' Plot rectangular regions from geometry data
#' 
#' @param rect_data Data with XMin, XMax, YMin, YMax columns
#' @param xlim X-axis limits (optional)
#' @param ylim Y-axis limits (optional)
#' @return ggplot object showing rectangles
#' @export
plot_rectangles <- function(rect_data, xlim = NULL, ylim = NULL) {
  p <- ggplot() + 
    theme_minimal() + 
    geom_rect(data = rect_data, 
              aes(xmin = XMin, xmax = XMax, ymin = YMin, ymax = YMax, color=!Exclude),
              fill = NA) +  # Hollow rectangles
    coord_fixed() +
    scale_y_reverse() +
    ggsci::scale_color_uchicago()
  
  # Apply zoom limits if provided
  if (!is.null(xlim) && !is.null(ylim)) {
    p <- p + coord_cartesian(xlim = xlim, ylim = ylim)
  }
  
  p
}

#' Plot both rectangular regions and cell points together
#' 
#' @param rect_data Rectangle data with XMin/XMax/YMin/YMax
#' @param cell_data Cell data with position columns
#' @param xlim X-axis limits (optional)
#' @param ylim Y-axis limits (optional) 
#' @param x_col Cell X coordinate column name
#' @param y_col Cell Y coordinate column name
#' @return Combined ggplot showing rectangles and points
#' @export
plot_cells_and_rectangles <- function(rect_data, cell_data, 
                                     xlim = NULL, ylim = NULL,
                                     x_col = "Xc", y_col = "Yc") {
  p <- ggplot() + 
    theme_minimal() + 
    # Draw rectangles first (background)
    geom_rect(data = rect_data, 
              aes(xmin = XMin, xmax = XMax, ymin = YMin, ymax = YMax, color=!Exclude),
              fill = NA) +
    ggsci::scale_color_uchicago() +
    # Add cell points on top
    geom_point(data = cell_data, aes(.data[[x_col]], .data[[y_col]]), fill="grey75")

  # Apply zoom if specified
  if (!is.null(xlim) && !is.null(ylim)) {
    p <- p + coord_fixed(xlim = xlim, ylim = ylim)
  } else {
    p <- p + coord_fixed()
  }
  
  p + scale_y_reverse()
}