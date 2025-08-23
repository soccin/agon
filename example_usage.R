# =============================================================================
# Example Usage of Spatial Analysis Functions
# =============================================================================
# This file demonstrates how to use the modular analysis functions
# for comparing Halo and Cortana cell data through spatial analysis
# =============================================================================

# Load the modular analysis functions
source("R/data_utils.R")      # Data loading and cleaning functions
source("R/plotting_utils.R")  # Plotting and visualization functions  
source("R/spatial_analysis.R") # Spatial analysis functions (loads the above two)

# =============================================================================
# SETUP: Define file paths and patterns
# =============================================================================

# Example file paths from your session history
csv_file <- "mpIF/GBM/10_samples/results/cell_table_with_exclusions/GBM_043___cell_table_size_normalized_final.csv"
rds_pattern <- "GBM_043"  # Pattern to match the corresponding Halo RDS file

# =============================================================================
# METHOD 1: Step-by-step analysis (gives you more control)
# =============================================================================

cat("=== METHOD 1: Step-by-step Analysis ===\n\n")

# Step 1: Load and clean the data
cat("Step 1: Loading and cleaning data...\n")
cell_data <- read_cell_table(csv_file) %>% clean_cell_data()
halo_data <- load_halo_data(rds_pattern)
geom_data <- extract_geom_data(halo_data)

cat("Cell data dimensions:", dim(cell_data), "\n")
cat("Geometry data dimensions:", dim(geom_data), "\n\n")

# Step 2: Create visualizations
cat("Step 2: Creating visualizations...\n")

# Basic scatter plot of all cells
p1 <- plot_cells_basic(cell_data)
print(p1)

# Plot just the rectangular regions (zoomed to interesting area)
p2 <- plot_rectangles(geom_data, xlim = c(2000, 3000), ylim = c(2000, 3000))
print(p2)

# Combined plot showing both cells and rectangles
p3 <- plot_cells_and_rectangles(geom_data, cell_data, 
                               xlim = c(2000, 3000), ylim = c(2000, 3000))
print(p3)

# Step 3: Perform spatial intersection analysis
cat("\nStep 3: Performing spatial intersection analysis...\n")
rect_sf <- create_rectangle_sf(geom_data)
points_sf <- create_points_sf(cell_data)
intersections <- find_intersections(rect_sf, points_sf)
stats <- analyze_intersection_stats(intersections)
mapping <- create_intersection_mapping(intersections)
one_to_one <- find_one_to_one_matches(mapping)

# Print results
cat("\n=== RESULTS ===\n")
cat("Points distribution (% of points intersecting N rectangles):\n")
print(stats$points_distribution)
cat("\nRectangles distribution (% of rectangles containing N points):\n")
print(stats$rectangles_distribution)
cat("\nOne-to-one matches found:", nrow(one_to_one), "\n\n")

# Create presentation-ready table
cat("=== PRESENTATION TABLE ===\n")
presentation_table <- format_stats_table(stats)
print(presentation_table)

# =============================================================================
# METHOD 2: All-in-one workflow function (simpler but less control)
# =============================================================================

cat("=== METHOD 2: Complete Workflow Function ===\n\n")

# Run the entire analysis in one function call
results <- analyze_spatial_data(csv_file, rds_pattern)

# Access the results (same as Method 1, but organized in a list)
cat("\n=== RESULTS FROM WORKFLOW ===\n")
cat("Points distribution:\n")
print(results$stats$points_distribution)
cat("\nRectangles distribution:\n")  
print(results$stats$rectangles_distribution)
cat("\nOne-to-one matches:", nrow(results$one_to_one_matches), "\n")

# Create plots from the results
plot_cells_basic(results$cell_data)
plot_cells_and_rectangles(results$geom_data, results$cell_data, 
                         xlim = c(2000, 3000), ylim = c(2000, 3000))

# =============================================================================
# ADDITIONAL ANALYSIS IDEAS
# =============================================================================

cat("\n=== Additional Analysis Examples ===\n")

# Example 1: Look at cells that are in multiple rectangles
multi_rect_points <- results$mapping$cortana_to_halo %>%
  rowwise() %>%
  mutate(n_rects = length(unlist(Halo))) %>%
  filter(n_rects > 1)

cat("Points in multiple rectangles:", nrow(multi_rect_points), "\n")

# Example 2: Look at rectangles with many points  
busy_rects <- results$mapping$halo_to_cortana %>%
  rowwise() %>%
  mutate(n_points = length(unlist(Cortana))) %>%
  arrange(desc(n_points)) %>%
  head(10)

cat("Top 10 rectangles by point count:\n")
print(busy_rects)

# Example 3: Focus on a specific region for detailed analysis
detailed_plot <- plot_cells_and_rectangles(results$geom_data, results$cell_data, 
                                          xlim = c(2000, 2500), ylim = c(2000, 2500))
print(detailed_plot)