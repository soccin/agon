# =============================================================================
# Spatial Analysis Functions
# =============================================================================
# This file contains functions for performing spatial intersection analysis
# between cell data points and rectangular regions using the sf package
# =============================================================================

library(tidyverse)
library(sf)        # For spatial data operations

# Load required utility modules
# Note: data_utils.R and plotting_utils.R are sourced from the main analysis script

# =============================================================================
# SPATIAL OBJECT CREATION FUNCTIONS  
# =============================================================================

#' Convert rectangle data to sf (simple features) spatial objects
#' 
#' Creates polygon geometries from XMin/XMax/YMin/YMax rectangle bounds.
#' Each rectangle is converted to a closed polygon with vertices ordered
#' counter-clockwise starting from bottom-left corner.
#' 
#' @param rect_data Data frame with rectangle bounds (XMin, XMax, YMin, YMax) and UUID column
#' @return sf object with polygon geometries indexed by UUID
#' @export
create_rectangle_sf <- function(rect_data) {
  rect_data %>%
    rowwise() %>%
    do({
      # Create polygon from rectangle corners (closed ring)
      # Vertices ordered counter-clockwise for proper geometry
      poly <- st_polygon(list(matrix(c(
        .$XMin, .$YMin,  # Bottom-left
        .$XMax, .$YMin,  # Bottom-right
        .$XMax, .$YMax,  # Top-right
        .$XMin, .$YMax,  # Top-left
        .$XMin, .$YMin   # Close the ring (return to start)
      ), ncol = 2, byrow = TRUE)))
      
      # Return data frame with UUID and geometry
      data.frame(UUID = .$UUID, geometry = st_sfc(poly))
    }) %>%
    st_sf()  # Convert to spatial features object
}

#' Convert point data to sf spatial objects
#' 
#' Creates point geometries from coordinate columns, retaining only the
#' specified ID column along with the geometry. Used to convert Cortana
#' cell data to spatial format for intersection analysis.
#' 
#' @param point_data Data frame with coordinate columns and cell identifiers
#' @param x_col X coordinate column name (default "Xc")
#' @param y_col Y coordinate column name (default "Yc")
#' @param id_col ID column to retain for matching (default "label")
#' @return sf point object with ID and geometry columns only
#' @export
create_points_sf <- function(point_data, x_col = "Xc", y_col = "Yc", id_col = "label") {
  # Convert to spatial points and keep only ID column
  st_as_sf(point_data, coords = c(x_col, y_col)) %>%
    select(all_of(id_col), geometry)  # Retain only identifier and spatial geometry
}

# =============================================================================
# INTERSECTION ANALYSIS FUNCTIONS
# =============================================================================

#' Find spatial intersections between rectangles and points
#' 
#' Performs bidirectional spatial intersection analysis using sf::st_intersects.
#' Returns sparse geometry binary predicate lists showing which spatial objects
#' intersect with each other in both directions (points-to-rectangles and 
#' rectangles-to-points).
#' 
#' @param rect_sf Rectangle sf object (Halo segmentation regions)
#' @param points_sf Points sf object (Cortana cell positions)
#' @return List with intersection results in both directions (sparse sgbp format)
#' @export
find_intersections <- function(rect_sf, points_sf) {
  list(
    # For each point (Cortana cell), which rectangles (Halo regions) contain it?
    points_in_rects = st_intersects(points_sf, rect_sf),
    # For each rectangle (Halo region), which points (Cortana cells) does it contain?
    rects_with_points = st_intersects(rect_sf, points_sf)
  )
}

#' Calculate statistics on intersection patterns
#' 
#' Analyzes the distribution of intersection multiplicities - how many objects
#' each spatial feature intersects with. Provides insight into data quality
#' and overlap patterns between Cortana cells and Halo segmentation regions.
#' 
#' @param intersections Result from find_intersections() containing sparse intersection lists
#' @return List with percentage distributions of intersection counts
#' @export
analyze_intersection_stats <- function(intersections) {
  # Count how many rectangles each point intersects (0, 1, 2, 3, ...)
  points_stats <- table(map_vec(intersections$points_in_rects, length))
  # Count how many points each rectangle contains (0, 1, 2, 3, ...)
  rects_stats <- table(map_vec(intersections$rects_with_points, length))
  
  list(
    # Convert counts to percentages for interpretability
    points_distribution = round(100 * points_stats / sum(points_stats), 2),
    rectangles_distribution = round(100 * rects_stats / sum(rects_stats), 2)
  )
}

#' Format intersection statistics as a presentation-ready table
#' 
#' Creates a nicely formatted tibble from intersection statistics with
#' standardized columns for both Cortana and Halo perspectives. Includes
#' descriptive labels for common intersection patterns and converts
#' percentages to proportions for easier interpretation.
#' 
#' @param stats Result from analyze_intersection_stats() with percentage distributions
#' @return Formatted tibble with intersection counts, descriptions, and proportions
#' @export
format_stats_table <- function(stats) {
  # Get the intersection counts observed in the data (0, 1, 2, 3, etc.)
  point_counts <- as.numeric(names(stats$points_distribution))
  rect_counts <- as.numeric(names(stats$rectangles_distribution))
  
  # Create complete range from 0 to maximum observed count
  all_counts <- 0:max(c(point_counts, rect_counts))
  
  # Create formatted table with standardized columns
  tibble(
    intersection_count = all_counts,
    # Convert percentages back to proportions, filling missing values with 0
    pct_cortana = map_dbl(all_counts, function(x) {
      val <- stats$points_distribution[as.character(x)]
      if (is.na(val)) 0.00 else val / 100
    }),
    pct_halo = map_dbl(all_counts, function(x) {
      val <- stats$rectangles_distribution[as.character(x)]
      if (is.na(val)) 0.00 else val / 100
    })
  ) %>%
    # Add interpretive descriptions for common patterns
    mutate(
      description = case_when(
        intersection_count == 0 ~ "No overlap",
        intersection_count == 1 ~ "Single match",
        intersection_count >= 2 ~ "Multiple matches"
      )
    ) %>%
    select(intersection_count, description, pct_cortana, pct_halo)
}

#' Create bidirectional mapping between points and rectangles
#' 
#' Transforms sparse intersection results into structured tibbles with
#' explicit mapping counts. Each row represents one spatial object with
#' its intersection count and list of matching objects. Essential for
#' one-to-one matching analysis.
#' 
#' @param intersections Result from find_intersections() with sparse intersection lists
#' @return List with Cortana->Halo and Halo->Cortana mapping tibbles
#' @export
create_intersection_mapping <- function(intersections) {
  # Cortana points to Halo rectangles mapping
  cortana_halo <- tibble(
    Cortana = seq_along(intersections$points_in_rects),  # Cortana cell index
    Nch = intersections$points_in_rects %>%              # Count of intersecting Halo regions
      map_vec(length),
    C2H = intersections$points_in_rects                  # List of Halo region indices
  )
  
  # Halo rectangles to Cortana points mapping
  halo_cortana <- tibble(
    Halo = seq_along(intersections$rects_with_points),   # Halo region index
    Nhc = intersections$rects_with_points %>%            # Count of intersecting Cortana cells
      map_vec(length),
    H2C = intersections$rects_with_points                # List of Cortana cell indices
  )
  
  list(
    cortana_to_halo = cortana_halo,
    halo_to_cortana = halo_cortana
  )
}

#' Find one-to-one matches between points and rectangles
#' 
#' Identifies perfect spatial correspondences where a Cortana cell is contained
#' in exactly one Halo region AND that Halo region contains exactly one Cortana cell.
#' This represents the ideal case for spatial comparison analysis. Includes
#' validation to ensure bidirectional consistency.
#' 
#' @param mapping Result from create_intersection_mapping() with bidirectional mappings
#' @return Data frame with one-to-one matches (Cortana and Halo indices)
#' @export
find_one_to_one_matches <- function(mapping) {
  # Find Cortana cells with exactly one Halo match
  xx=mapping$cortana_to_halo %>%
    filter(Nch==1) %>%          # Cortana cell intersects exactly 1 Halo region
    unnest(C2H) %>%             # Extract the single Halo region index
    # Join with reverse mapping to check if Halo region is also exclusive
    left_join(mapping$halo_to_cortana,by=c(C2H="Halo")) %>%
    filter(Nhc==1) %>%          # Halo region contains exactly 1 Cortana cell
    unnest(H2C) %>%             # Extract the single Cortana cell index
    rename(Halo=C2H)
  
  # Validation: ensure bidirectional consistency (Cortana index should match H2C)
  n_missMatch <- xx %>% filter(Cortana!=H2C) %>% nrow
  if(n_missMatch>0) {
    rlang::abort("FATAL ERROR::Bidirectional mapping inconsistency detected")
  }
  
  # Return clean one-to-one matches
  xx %>% select(Cortana,Halo)
}

# =============================================================================
# COMPLETE WORKFLOW WRAPPER FUNCTION
# =============================================================================

#' Complete spatial analysis workflow (DEPRECATED)
#' 
#' This function represents the original workflow design but is no longer used
#' in the current analysis pipeline. The workflow has been refactored to handle
#' data loading externally and process individual FOVs separately.
#' 
#' @param csv_file Path to cell table CSV file
#' @param rds_pattern Pattern to match Halo RDS file
#' @return List containing all analysis results
#' @deprecated Use individual functions in the current FOV-based analysis pipeline
#' @export
analyze_spatial_data <- function(csv_file, rds_pattern) {
  # DEPRECATED: This monolithic workflow has been replaced by FOV-based processing
  
  # Step 1: Load and clean data
  cat("Loading cell data from CSV...\n")
  cell_data <- read_cell_table(csv_file) %>%
  clean_cell_data()
  
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