# Halo-Cortana Spatial Analysis Comparison

A modular R toolkit for comparing spatial cell segmentation results between Halo and Cortana (Mesmer) platforms through geometric intersection analysis.

## Overview

This project provides a comprehensive framework for analyzing and comparing cell segmentation outputs from two different platforms:
- **Halo**: Pathology image analysis platform with rectangular region annotations
- **Cortana (Mesmer)**: Deep learning-based cell segmentation producing point coordinates

The toolkit performs spatial intersection analysis to identify overlapping regions and generate statistics on segmentation agreement between the two platforms.

## Features

- **Modular Architecture**: Clean separation of data loading, visualization, and analysis functions
- **Spatial Analysis**: Uses `sf` (Simple Features) for robust geometric operations
- **Visualization**: ggplot2-based plotting with color coding for included/excluded regions  
- **Statistical Output**: Formatted tables ready for presentation in PowerPoint
- **Batch Processing**: Command-line script for automated analysis of multiple samples

## Installation

### Prerequisites

Required R packages:
```r
install.packages(c("tidyverse", "sf", "ggsci", "fs", "patchwork"))
```

### Setup

1. Clone or download this repository
2. Ensure your data follows the expected directory structure:
   ```
   data/
   ├── mpIF/GBM/10_samples/results/cell_table_with_exclusions/
   │   └── [SAMPLE_ID]___cell_table_size_normalized_final.csv.gz
   └── Halo/
       └── [SAMPLE_ID]___Halo_*.rda
   ```

## Usage

### Interactive Analysis

Load the modular functions:
```r
source("data_utils.R")      # Data loading and cleaning
source("plotting_utils.R")  # Visualization functions  
source("spatial_analysis.R") # Spatial analysis (auto-loads above)
```

Basic workflow:
```r
# Load and clean data
sample_id <- "GBM_043"
cell_data <- read_cell_table(sample_id) %>% clean_cell_data()
halo_data <- load_halo_data(sample_id)
geom_data <- extract_geom_data(halo_data)

# Create visualizations
plot_cells_basic(cell_data)
plot_cells_and_rectangles(geom_data, cell_data)

# Perform spatial analysis
results <- analyze_spatial_data("path/to/csv", sample_id)
presentation_table <- format_stats_table(results$stats)
```

### Batch Processing

For automated analysis of multiple samples:
```bash
Rscript analysis01.R SAMPLE_ID
```

This generates:
- Multi-page PDF with comparison plots at different zoom levels
- CSV file with intersection statistics
- Formatted statistics table

## Module Structure

### `data_utils.R`
- `read_cell_table()`: Load CSV files with automatic pattern matching
- `clean_cell_data()`: Standardize column selection and FOV formatting
- `load_halo_data()`: Load RDS files from Halo platform
- `extract_geom_data()`: Extract geometry data with coordinate columns

### `plotting_utils.R`  
- `plot_cells_basic()`: Basic scatter plot of cell positions
- `plot_rectangles()`: Visualization of rectangular regions with exclusion coloring
- `plot_cells_and_rectangles()`: Combined overlay plots

### `spatial_analysis.R`
- `create_rectangle_sf()`, `create_points_sf()`: Convert to spatial objects
- `find_intersections()`: Perform geometric intersection analysis
- `analyze_intersection_stats()`: Calculate percentage distributions
- `format_stats_table()`: Create presentation-ready output tables
- `analyze_spatial_data()`: Complete workflow wrapper function

## Output Interpretation

The analysis produces intersection statistics showing:
- **Points distribution**: Percentage of Cortana points intersecting 0, 1, 2+ Halo rectangles
- **Rectangles distribution**: Percentage of Halo rectangles containing 0, 1, 2+ Cortana points

Example output:
```
Intersection Count | Description      | Points (%) | Rectangles (%)
0                 | No overlap       | 13.02      | 4.80
1                 | Single match     | 82.77      | 88.86  
2+                | Multiple matches | 4.21       | 6.34
```

## Version History

- **v1.0.1**: Initial release with modular architecture and batch processing
- Modularized from interactive R session analysis
- Added presentation-ready table formatting
- Enhanced visualizations with color coding

## Requirements

- R ≥ 4.0
- Required packages: tidyverse, sf, ggsci, fs, patchwork
- Data in specified directory structure

## License

Internal research tool - contact repository owner for usage permissions.

## Contributing

This is an active research project. For questions or contributions, please contact the development team.