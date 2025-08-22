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
#' @param sample_id (e.g., "GBM_043")
#' @return Tibble with cell data
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
    rename(FOV=fov) %>%
    mutate(FOV=as.numeric(gsub("^R","",FOV)))
}

#' Load and clean cell data in one step
#' 
#' Convenience function that reads cell table CSV file and applies cleaning 
#' transformations. Combines read_cell_table() and clean_cell_data() operations.
#' 
#' @param sample_id Sample identifier (e.g., "GBM_043")
#' @return Cleaned tibble with cell data
#' @export
load_cortana_data <- function(sample_id) {
  read_cell_table(sample_id) |> clean_cell_data()
}

# =============================================================================
# RDS FILE HANDLING FUNCTIONS
# =============================================================================

#' Load Halo data from RDS file matching a pattern
#' 
#' @param sample_id (e.g., "GBM_043")
#' @return Loaded Halo data object
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
#' Selects UUID, FOV, and all X/Y coordinate columns
#' 
#' @param halo_data Loaded Halo data object
#' @return Tibble with geometry information
#' @export
extract_geom_data <- function(halo_data) {
  halo_data$geom.data %>% 
    select(UUID, FOV, matches("^[XY]"),Exclude)  # UUID, FOV, and X/Y coordinates
}