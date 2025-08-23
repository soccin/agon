# =============================================================================
# Data Loading and Cleaning Utilities
# =============================================================================
# This file contains functions for loading and cleaning cell data from
# CSV files and RDS files containing Halo geometry data
# =============================================================================

suppressPackageStartupMessages({library(tidyverse)})

# =============================================================================
# CSV DATA FUNCTIONS
# =============================================================================

#' Load cell table CSV file
#' 
#' Searches for and loads cell table CSV file matching the pattern
#' "sample_id___cell_table_size_normalized_final.csv" within the
#' data/mpIF directory structure using recursive search.
#' 
#' @param sample_id Sample identifier string (e.g., "GBM_043")
#' @return Tibble with raw cell data from CSV file
#' @export
read_cell_table <- function(sample_id) {
  mesmer_folder="data/mpIF"
  read_csv(fs::dir_ls(
            mesmer_folder,
            recur=T,
            regex=paste0("/",sample_id,"___cell_table_size_normalized_final.csv")
            )
  )
}

#' Clean cell data by selecting relevant columns and removing nuclear markers
#' 
#' Keeps: label through cell_size, area through end, removes _nuclear columns
#' 
#' @param data Raw cell data tibble
#' @return Cleaned tibble with selected columns
#' @export
clean_cell_data <- function(data) {
  data %>% 
    select(label:cell_size, area:ncol(data)) %>%  # Select core columns
    select(-matches("_nuclear")) %>%              # Remove nuclear-specific columns
    rename(FOV=fov)
}

#' Add an Exclude flag field that matches the Halo convention
#' 
#' Creates a boolean Exclude column based on layer exclusion criteria.
#' Cells are marked for exclusion if they have invalid measurements 
#' (value of -1) in nuclear or whole cell layer 1, or if they have 
#' positive values (indicating presence) in exclusion layers 2.
#' 
#' @param cortana_data Cleaned cell data tibble from clean_cell_data()
#' @return Tibble with additional Exclude column (logical)
#' @export
add_exclude_flag <- function(cortana_data) {
  cortana_data %>%
    mutate(
      Exclude=nuclear_layer_1==-1
        | whole_cell_layer_1==-1
        | nuclear_layer_2==1
        | whole_cell_layer_2==1
        )
}

#' Load and clean cell data in one step
#' 
#' Convenience function that reads cell table CSV file and applies cleaning 
#' transformations. Combines read_cell_table(), clean_cell_data(), and
#' add_exclude_flag() operations to provide fully processed cell data.
#' 
#' @param sample_id Sample identifier string (e.g., "GBM_043")
#' @return Cleaned tibble with cell data and Exclude flag
#' @export
load_cortana_data <- function(sample_id) {
  read_cell_table(sample_id) |>
    clean_cell_data() |>
    add_exclude_flag()
}

# =============================================================================
# RDS FILE HANDLING FUNCTIONS
# =============================================================================

#' Load Halo data from RDS file matching a pattern
#' 
#' Searches for and loads RDS file matching the pattern
#' "sample_id___Halo*.rda" within the data/Halo directory.
#' If multiple files match, loads the first one found.
#' 
#' @param sample_id Sample identifier string (e.g., "GBM_043")
#' @return Loaded Halo data object from RDS file
#' @export
load_halo_data <- function(sample_id) {
  rds_file <- fs::dir_ls("data/Halo",regex=paste0("/",sample_id,"___Halo.*rda"))
  if (length(rds_file) == 0) {
    stop("No RDS file found matching pattern: ", pattern)
  }
  readRDS(rds_file[1])  # Load first matching file
}

#' Extract geometry data from Halo data object
#' 
#' Selects UUID, FOV, Exclude flag, and all X/Y coordinate columns
#' from the geom.data component. Formats FOV as "R###" string format
#' with zero-padding (e.g., "R001", "R002").
#' 
#' @param halo_data Loaded Halo data object with geom.data component
#' @return Tibble with UUID, formatted FOV, coordinates, and Exclude flag
#' @export
extract_geom_data <- function(halo_data) {
  halo_data$geom.data %>% 
    select(UUID, FOV, matches("^[XY]"),Exclude)  %>% # UUID, FOV, and X/Y coordinates
    mutate(FOV=sprintf("R%03d",FOV))
}