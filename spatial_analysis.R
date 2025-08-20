# =============================================================================
# Spatial Analysis Functions
# =============================================================================
# This file contains functions for performing spatial intersection analysis
# between cell data points and rectangular regions using the sf package
# =============================================================================

library(tidyverse)
library(sf)        # For spatial data operations

# Load required utility modules
source("data_utils.R")
source("plotting_utils.R")

# =============================================================================
# SPATIAL OBJECT CREATION FUNCTIONS  
# =============================================================================

#' Convert rectangle data to sf (simple features) spatial objects
#' 
#' Creates polygon geometries from XMin/XMax/YMin/YMax bounds
#' 
#' @param rect_data Data frame with rectangle bounds and UUID
#' @return sf object with polygon geometries
#' @export
create_rectangle_sf <- function(rect_data) {
  rect_data %>%
    rowwise() %>%
    do({
      # Create polygon from rectangle corners (closed ring)
      poly <- st_polygon(list(matrix(c(
        .$XMin, .$YMin,  # Bottom-left
        .$XMax, .$YMin,  # Bottom-right
        .$XMax, .$YMax,  # Top-right
        .$XMin, .$YMax,  # Top-left
        .$XMin, .$YMin   # Close the ring
      ), ncol = 2, byrow = TRUE)))
      
      data.frame(UUID = .$UUID, geometry = st_sfc(poly))
    }) %>%
    st_sf()  # Convert to spatial features
}

#' Convert point data to sf spatial objects
#' 
#' @param point_data Data frame with coordinate columns
#' @param x_col X coordinate column name  
#' @param y_col Y coordinate column name
#' @param id_col ID column to retain
#' @return sf point object
#' @export
create_points_sf <- function(point_data, x_col = "Xc", y_col = "Yc", id_col = "label") {
  st_as_sf(point_data, coords = c(x_col, y_col)) %>%
    select(all_of(id_col), geometry)
}

# =============================================================================
# INTERSECTION ANALYSIS FUNCTIONS
# =============================================================================

#' Find spatial intersections between rectangles and points
#' 
#' @param rect_sf Rectangle sf object
#' @param points_sf Points sf object  
#' @return List with intersection results in both directions
#' @export
find_intersections <- function(rect_sf, points_sf) {
  list(
    # For each point, which rectangles contain it?
    points_in_rects = st_intersects(points_sf, rect_sf),
    # For each rectangle, which points does it contain?
    rects_with_points = st_intersects(rect_sf, points_sf)
  )
}

#' Calculate statistics on intersection patterns
#' 
#' @param intersections Result from find_intersections()
#' @return List with percentage distributions
#' @export
analyze_intersection_stats <- function(intersections) {
  # Count how many rectangles each point intersects
  points_stats <- table(map_vec(intersections$points_in_rects, length))
  # Count how many points each rectangle contains  
  rects_stats <- table(map_vec(intersections$rects_with_points, length))
  
  list(
    # Convert to percentages
    points_distribution = round(100 * points_stats / sum(points_stats), 2),
    rectangles_distribution = round(100 * rects_stats / sum(rects_stats), 2)
  )
}

#' Format intersection statistics as a presentation-ready table
#' 
#' Creates a nicely formatted tibble from intersection statistics
#' that can be easily copied to PowerPoint or other presentations
#' 
#' @param stats Result from analyze_intersection_stats()
#' @return Formatted tibble with both distributions
#' @export
format_stats_table <- function(stats) {
  # Get the intersection counts (0, 1, 2, 3, etc.)
  point_counts <- as.numeric(names(stats$points_distribution))
  rect_counts <- as.numeric(names(stats$rectangles_distribution))
  
  # Create all possible counts from 0 to maximum
  all_counts <- 0:max(c(point_counts, rect_counts))
  
  # Create formatted table
  tibble(
    `Intersection Count` = all_counts,
    `Points (%)` = map_chr(all_counts, function(x) {
      val <- stats$points_distribution[as.character(x)]
      if (is.na(val)) "0.00" else sprintf("%.2f", val)
    }),
    `Rectangles (%)` = map_chr(all_counts, function(x) {
      val <- stats$rectangles_distribution[as.character(x)]
      if (is.na(val)) "0.00" else sprintf("%.2f", val)
    })
  ) %>%
    # Add descriptive labels
    mutate(
      `Description` = case_when(
        `Intersection Count` == 0 ~ "No overlap",
        `Intersection Count` == 1 ~ "Single match", 
        `Intersection Count` >= 2 ~ "Multiple matches"
      )
    ) %>%
    select(`Intersection Count`, Description, `Points (%)`, `Rectangles (%)`)
}

#' Create bidirectional mapping between points and rectangles
#' 
#' @param intersections Result from find_intersections()
#' @return List with Cortana->Halo and Halo->Cortana mappings
#' @export
create_intersection_mapping <- function(intersections) {
  # Cortana points to Halo rectangles
  cortana_halo <- tibble(
    Cortana = seq_along(intersections$points_in_rects),
    Halo = map(intersections$points_in_rects, list)
  )
  
  # Halo rectangles to Cortana points
  halo_cortana <- tibble(
    Halo = seq_along(intersections$rects_with_points),
    Cortana = map(intersections$rects_with_points, list)
  )
  
  list(
    cortana_to_halo = cortana_halo,
    halo_to_cortana = halo_cortana
  )
}

#' Find one-to-one matches between points and rectangles
#' 
#' Identifies cases where a point is in exactly one rectangle
#' and that rectangle contains exactly one point
#' 
#' @param mapping Result from create_intersection_mapping()
#' @return Data frame with one-to-one matches
#' @export
find_one_to_one_matches <- function(mapping) {
  full_join(mapping$halo_to_cortana, 
            mapping$cortana_to_halo %>% unnest(Halo), 
            by = "Halo") %>%
    filter(!is.na(Cortana.y)) %>%           # Remove unmatched rectangles
    group_by(Cortana.y) %>%                 # Group by Cortana points
    filter(n() == 1) %>%                    # Keep points in exactly 1 rectangle
    group_by(Halo) %>%                      # Group by Halo rectangles  
    filter(n() == 1) %>%                    # Keep rectangles with exactly 1 point
    unnest(Cortana.x)                       # Expand the mapping
}

# =============================================================================
# COMPLETE WORKFLOW WRAPPER FUNCTION
# =============================================================================

#' Complete spatial analysis workflow
#' 
#' Loads data, creates spatial objects, performs intersection analysis
#' 
#' @param csv_file Path to cell table CSV file
#' @param rds_pattern Pattern to match Halo RDS file
#' @return List containing all analysis results
#' @export
analyze_spatial_data <- function(csv_file, rds_pattern) {
  # Step 1: Load and clean data
  cat("Loading cell data from CSV...\n")
  cell_data <- read_cell_table(csv_file) %>% clean_cell_data()
  
  cat("Loading Halo geometry data...\n")
  halo_data <- load_halo_data(rds_pattern)
  geom_data <- extract_geom_data(halo_data)
  
  # Step 2: Create spatial objects
  cat("Creating spatial objects...\n")
  rect_sf <- create_rectangle_sf(geom_data)
  points_sf <- create_points_sf(cell_data)
  
  # Step 3: Perform intersection analysis
  cat("Finding spatial intersections...\n")
  intersections <- find_intersections(rect_sf, points_sf)
  stats <- analyze_intersection_stats(intersections)
  mapping <- create_intersection_mapping(intersections)
  one_to_one <- find_one_to_one_matches(mapping)
  
  cat("Analysis complete!\n")
  cat("One-to-one matches found:", nrow(one_to_one), "\n")
  
  # Return all results in a structured list
  list(
    cell_data = cell_data,              # Cleaned Cortana cell data
    geom_data = geom_data,              # Halo geometry data
    rect_sf = rect_sf,                  # Rectangle spatial objects
    points_sf = points_sf,              # Point spatial objects  
    intersections = intersections,       # Raw intersection results
    stats = stats,                      # Summary statistics
    mapping = mapping,                  # Bidirectional mappings
    one_to_one_matches = one_to_one     # Perfect 1:1 matches
  )
}