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
  pattern <- paste0("/",sample_id,"___cell_table_size_normalized_final.csv")
  matching_files <- fs::dir_ls(mesmer_folder, recur=T, regex=pattern)
  
  if (length(matching_files) == 0) {
    stop("No cell table CSV file found for sample '", sample_id, "'. ",
         "Expected pattern: *", sample_id, "___cell_table_size_normalized_final.csv ",
         "in directory: ", mesmer_folder)
  }

  if(len(matching_files)>1) {
    stop("Multiple cell files find mathing sample name",sample_id)
  }
  
  cat("Loading cell file",matching_files,"for sample_id",sample_id,"\n")

  read_csv(matching_files)
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

#' Add exclusion flags and categorization for cell data
#' 
#' Creates multiple exclusion-related columns:
#' - Exclude: Boolean flag marking cells for exclusion based on whole cell layer criteria
#' - ExcVal: Numeric exclusion value (0=not excluded, 1=outside, 2=inside exclusion zones)
#' - ExcDesc: Factor with descriptive exclusion categories ("not", "outside", "inside")
#' 
#' Exclusion criteria:
#' - whole_cell_layer_1 == -1: Invalid measurement (outside detection area)
#' - whole_cell_layer_2 == 1: Inside exclusion zone
#' 
#' @param cortana_data Cleaned cell data tibble from clean_cell_data()
#' @return Tibble with additional Exclude (logical), ExcVal (numeric), ExcDesc (factor) columns
#' @export
add_exclude_flag <- function(cortana_data) {
  cortana_data %>%
    mutate(
      # Primary exclusion flag: cells with invalid measurements or in exclusion zones
      Exclude=
        whole_cell_layer_1==-1    # Outside detection area
        | whole_cell_layer_2==1   # Inside exclusion zone
        ) %>%
    # Calculate numeric exclusion value based on layer status
    # Formula combines layer 1 (detection) and layer 2 (exclusion) information
    mutate(ExcVal=(2*(sign(whole_cell_layer_2)+1)/2+(-whole_cell_layer_1+1)/2)) %>%
    # Convert numeric codes to descriptive factor labels
    mutate(ExcDesc=fct_recode(factor(ExcVal),"not"="0","outside"="1","inside"="2"))
}

#' Load and clean cell data in one step
#' 
#' Convenience function that reads cell table CSV file and applies all cleaning 
#' transformations. Combines read_cell_table(), clean_cell_data(), and
#' add_exclude_flag() operations to provide fully processed cell data with
#' exclusion categorization.
#' 
#' @param sample_id Sample identifier string (e.g., "GBM_043")
#' @return Cleaned tibble with cell data, Exclude flag, ExcVal, and ExcDesc columns
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
  pattern <- paste0("/",sample_id,"___Halo.*rda")
  rds_file <- fs::dir_ls("data/Halo", regex=pattern)
  
  if (length(rds_file) == 0) {
    stop("No Halo RDS file found for sample '", sample_id, "'. ",
         "Expected pattern: *", sample_id, "___Halo*.rda ",
         "in directory: data/Halo")
  }
  
  if(len(rds_file)>1) {
    print(str(rds_file))
    stop("To many matches",len(rds_file),"for sample_id",sample_id)
  }

  cat("Loading Halo file",rds_file,"\n")
  readRDS(rds_file)
}

#' Extract and process geometry data from Halo data object
#' 
#' Processes Halo geometry data to create a unified exclusion categorization system.
#' Selects UUID, FOV, and X/Y coordinates, then consolidates multiple Exclude.* 
#' columns into a single ExcDesc column with semicolon-separated exclusion types.
#' Formats FOV as "R###" string with zero-padding (e.g., "R001", "R002").
#' 
#' Processing steps:
#' 1. Extract basic geometry data (UUID, FOV, coordinates)
#' 2. Gather all Exclude.* columns and filter for TRUE flags  
#' 3. Concatenate exclusion types per UUID (e.g., "1;3" for multiple exclusions)
#' 4. Map to descriptive categories ("0" for no exclusions)
#' 
#' @param halo_data Loaded Halo data object with geom.data component containing Exclude.* columns
#' @return Tibble with UUID, formatted FOV, coordinates, and consolidated ExcDesc column
#' @export
extract_geom_data <- function(halo_data) {
  # Extract basic geometry data: UUID, FOV, coordinates, and all exclusion columns
  gg=halo_data$geom.data %>%
    select(UUID, FOV, matches("^[XY]|Exclude"))  %>% # UUID, FOV, X/Y coordinates, Exclude.*
    mutate(FOV=sprintf("R%03d",FOV))  # Format FOV as R001, R002, etc.

  # Process exclusion flags: consolidate multiple Exclude.* columns per UUID
  ee=gg %>%
    select(UUID,matches("Exclude\\.")) %>%        # Select only Exclude.* columns
    gather(Exclude,Flag,-UUID) %>%                # Pivot to long format
    group_by(UUID) %>%
    mutate(Exclude=gsub("Exclude.","",Exclude)) %>%  # Remove "Exclude." prefix
    filter(Flag) %>%                              # Keep only TRUE exclusion flags
    summarize(Flag=paste0(sort(Exclude),collapse=";"))  # Concatenate exclusion types

  # Merge exclusion summary back with geometry data
  left_join(gg,ee) %>%
    mutate(Flag=ifelse(is.na(Flag),"0",Flag)) %>%  # "0" for cells with no exclusions
    rename(ExcDesc=Flag)  # Rename to match plotting function expectations
}