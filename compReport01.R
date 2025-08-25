# ==============================================================================
# Halo vs Cortana Comparison Report
# ==============================================================================
# Analysis of spatial overlap between Halo and Cortana platforms with QC metrics
# Generates scatter plot showing platform concordance colored by exclusion rate
# ==============================================================================

# Load required libraries
require(tidyverse)
require(scales)

# ==============================================================================
# Configuration and Setup
# ==============================================================================

# Define color palette for visualization (turbo colormap)
turbo_colors <- c("#30123b", "#4662d7", "#36a1f4", "#13c5dd",
                  "#34e8b0", "#a4fb64", "#fee838", "#fb8022",
                  "#d23105", "#7a0403")

# ==============================================================================
# Data Loading and Processing
# ==============================================================================

# Load FOV exclusion criteria from Excel file
# Filter for FOVs with exclusion reasons and format FOV names
exc_fovs <- readxl::read_xlsx("GBM_FOVs__V6.xlsx") %>% 
  filter(!is.na(FOV_exclusion)) %>% 
  mutate(fov = sprintf("R%03d", FOV_number)) %>% 
  select(sample_id = CellDive_ID, fov, everything())

# Load overlap statistics from all files matching pattern
# Combine all overlap stat files into single dataframe
overlap_stats <- fs::dir_ls("output/stats", regex = "overlapStats") %>% 
  map_dfr(read_csv, show_col_types = FALSE, progress = FALSE, .progress = TRUE)

# Load exclusion statistics from all files matching pattern  
# Combine all exclusion stat files into single dataframe
exclusion_stats <- fs::dir_ls("output/stats", regex = "excStats_") %>% 
  map_dfr(read_csv, show_col_types = FALSE, progress = FALSE, .progress = TRUE)

# ==============================================================================
# Quality Control Analysis
# ==============================================================================

# Filter overlap data for quality control
# Remove entries with missing intersection counts (failed analyses)
qc_data <- overlap_stats %>% 
  filter(is.na(intersection_count)) %>% 
  select(-intersection_count, -description)

# Merge with exclusion statistics and calculate exclusion percentage
# Select relevant columns and sort by percentage of valid cells (pct_0)
qc_data <- qc_data %>% 
  left_join(exclusion_stats) %>% 
  select(sample_id:pct_0, matches("pct.*Drift")) %>% 
  mutate(pct_exc = 1 - pct_0) %>% 
  arrange(pct_0)

write_xlsx(qc_data,"qcData_CvH_250824.xlsx")

qc_data=qc_data %>%
  mutate(label=case_when(
        pct_halo<.4 ~ paste0(sample_id,":",fov),
        pct_exc>.97 ~ paste0(sample_id,":",fov), T ~ "")) %>%
  mutate(label=gsub("GBM.","",label))

# ==============================================================================
# Visualization
# ==============================================================================

# Create scatter plot comparing Halo vs Cortana percentages
# Color and size points by exclusion rate, add sample ID labels
comparison_plot <- qc_data %>% 
  mutate(pct_exc = 1 - pct_0) %>% 
  ggplot(aes(x = pct_halo, y = pct_cortana, 
             color = pct_exc, size = pct_exc, 
             label = label)) +
  theme_light(20) + 
  geom_point(alpha = 0.5) + 
  scale_color_gradientn(colors = turbo_colors, 
                        name = "Exclusion\nRate") + 
  scale_size_continuous(name = "Exclusion\nRate") +
  scale_x_continuous(labels = percent_format(), 
                     limits = c(0, 1), 
                     breaks = seq(0, 1, 0.2),
                     name = "Halo Platform (%)") + 
  scale_y_continuous(labels = percent_format(), 
                     limits = c(0, 1), 
                     breaks = seq(0, 1, 0.2),
                     name = "Cortana Platform (%)") + 
  coord_fixed() +
  ggrepel::geom_label_repel(size = 3, color = "grey35", max.overlaps=20, force=5, force_pull=0.1, min.segment.length = 0) +
  labs(title = "Halo vs Cortana Platform Comparison",
       subtitle = "Points colored and sized by exclusion rate")

# Save plot to PDF
pdf(file = "pltComp01.pdf", width = 11, height = 11)
print(comparison_plot)
dev.off()

#ggrepel::geom_label_repel(size = 5, color = "grey35", box.padding=.5) +
