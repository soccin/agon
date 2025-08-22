# =============================================================================
# Data Loading and Cleaning Utilities with S7 Objects
# =============================================================================
# This file contains S7 classes for loading and cleaning cell data from
# CSV files and RDS files containing Halo geometry data
# =============================================================================

library(tidyverse)
library(S7)

# =============================================================================
# S7 CLASS DEFINITIONS
# =============================================================================

#' Cell Data Manager Class
#' 
#' Manages loading and cleaning of cell data from CSV files
cell_data_manager <- new_class("cell_data_manager",
  properties = list(
    mesmer_folder = class_character,
    data = class_any
  )
)

#' Halo Data Manager Class
#' 
#' Manages loading and processing of Halo geometry data from RDS files
halo_data_manager <- new_class("halo_data_manager",
  properties = list(
    halo_folder = class_character,
    data = class_any
  )
)

# =============================================================================
# CONSTRUCTORS
# =============================================================================

#' Create Cell Data Manager
#' 
#' @param mesmer_folder Path to folder containing cell CSV files
#' @return cell_data_manager object
cell_data_manager <- function(mesmer_folder = "data/mpIF/GBM/10_samples/results/cell_table_with_exclusions") {
  if (!dir.exists(mesmer_folder)) {
    cli::cli_abort("Mesmer folder does not exist: {mesmer_folder}")
  }
  new_object(cell_data_manager, 
             mesmer_folder = mesmer_folder, 
             data = NULL)
}

#' Create Halo Data Manager
#' 
#' @param halo_folder Path to folder containing Halo RDS files
#' @return halo_data_manager object
halo_data_manager <- function(halo_folder = "data/Halo") {
  if (!dir.exists(halo_folder)) {
    cli::cli_abort("Halo folder does not exist: {halo_folder}")
  }
  new_object(halo_data_manager, 
             halo_folder = halo_folder, 
             data = NULL)
}

# =============================================================================
# CELL DATA METHODS
# =============================================================================

#' Load cell table CSV file
#' 
#' @param x cell_data_manager object
#' @param sample_id Sample identifier (e.g., "GBM_043")
#' @return cell_data_manager object with loaded data
method(load_data, cell_data_manager) <- function(x, sample_id) {
  file_pattern <- paste0("/", sample_id, "___cell.*csv.gz$")
  files <- fs::dir_ls(x@mesmer_folder, regex = file_pattern)
  
  if (length(files) == 0) {
    cli::cli_abort("No cell data file found for sample: {sample_id}")
  }
  
  raw_data <- read_csv(files[1], show_col_types = FALSE)
  x@data <- raw_data
  x
}

#' Clean cell data by selecting relevant columns and removing nuclear markers
#' 
#' Keeps: label through cell_size, area through end, removes _nuclear columns
#' 
#' @param x cell_data_manager object with loaded data
#' @return cell_data_manager object with cleaned data
method(clean_data, cell_data_manager) <- function(x) {
  if (is.null(x@data)) {
    cli::cli_abort("No data loaded. Use load_data() first.")
  }
  
  cleaned_data <- x@data %>% 
    select(label:cell_size, area:ncol(x@data)) %>%  # Select core columns
    select(-matches("_nuclear")) %>%                # Remove nuclear-specific columns
    rename(FOV = fov) %>%
    mutate(FOV = as.numeric(gsub("^R", "", FOV)))
  
  x@data <- cleaned_data
  x
}

#' Get cleaned cell data
#' 
#' @param x cell_data_manager object
#' @return Tibble with cell data
method(get_data, cell_data_manager) <- function(x) {
  x@data
}

# =============================================================================
# HALO DATA METHODS
# =============================================================================

#' Load Halo data from RDS file matching a pattern
#' 
#' @param x halo_data_manager object
#' @param sample_id Sample identifier (e.g., "GBM_043")
#' @return halo_data_manager object with loaded data
method(load_data, halo_data_manager) <- function(x, sample_id) {
  file_pattern <- paste0("/", sample_id, "___Halo.*rda")
  files <- fs::dir_ls(x@halo_folder, regex = file_pattern)
  
  if (length(files) == 0) {
    cli::cli_abort("No Halo data file found for sample: {sample_id}")
  }
  
  halo_data <- readRDS(files[1])
  x@data <- halo_data
  x
}

#' Extract geometry data from Halo data object
#' 
#' Selects UUID, FOV, and all X/Y coordinate columns
#' 
#' @param x halo_data_manager object with loaded data
#' @return halo_data_manager object with extracted geometry data
method(extract_geom, halo_data_manager) <- function(x) {
  if (is.null(x@data)) {
    cli::cli_abort("No data loaded. Use load_data() first.")
  }
  
  geom_data <- x@data$geom.data %>% 
    select(UUID, FOV, matches("^[XY]"), Exclude)  # UUID, FOV, and X/Y coordinates
  
  x@data <- geom_data
  x
}

#' Get geometry data
#' 
#' @param x halo_data_manager object
#' @return Tibble with geometry data
method(get_data, halo_data_manager) <- function(x) {
  x@data
}

# =============================================================================
# CONVENIENCE FUNCTIONS (for backward compatibility)
# =============================================================================

#' Load and clean cell data (convenience function)
#' 
#' @param sample_id Sample identifier (e.g., "GBM_043")
#' @param mesmer_folder Path to folder containing cell CSV files
#' @return Tibble with cleaned cell data
read_cell_table <- function(sample_id, mesmer_folder = "data/mpIF/GBM/10_samples/results/cell_table_with_exclusions") {
  cell_manager <- cell_data_manager(mesmer_folder)
  cell_manager %>%
    load_data(sample_id) %>%
    clean_data() %>%
    get_data()
}

#' Load and extract Halo geometry data (convenience function)
#' 
#' @param sample_id Sample identifier (e.g., "GBM_043")
#' @param halo_folder Path to folder containing Halo RDS files
#' @return Tibble with geometry data
load_halo_geom <- function(sample_id, halo_folder = "data/Halo") {
  halo_manager <- halo_data_manager(halo_folder)
  halo_manager %>%
    load_data(sample_id) %>%
    extract_geom() %>%
    get_data()
}