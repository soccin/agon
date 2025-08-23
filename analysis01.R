# =============================================================================
# Spatial Analysis Pipeline: Cortana vs Halo Comparison
# =============================================================================
# 
# This script performs comprehensive spatial overlap analysis between Cortana 
# cell segmentation data and Halo region-of-interest (ROI) annotations for a 
# given sample. It generates intersection statistics, identifies one-to-one 
# matches, and produces diagnostic plots for quality assessment.
#
# Input: Sample identifier (e.g., "GBM_043")
# Output: 
#   - Overlap statistics CSV (output/stats/overlapStats_<sample>.csv)
#   - Exclusion statistics CSV (output/stats/excStats_<sample>.csv)  
#   - Diagnostic plots for low-quality FOVs (output/plots/)
#
# Usage: Rscript analysis01.R <SAMPLE_ID>
# =============================================================================

argv=commandArgs(trailing=T)

if(len(argv)!=1) {
  cat("
  usage: Rscript analysis01.R SAMPLE_ID
     eg: Rscript analysis01.R GBM_043\n\n")
  quit()
}

SDIR=get_script_dir()

source(file.path(SDIR,"R/data_utils.R"))
source(file.path(SDIR,"R/plotting_utils.R"))
source(file.path(SDIR,"R/spatial_analysis.R"))
source(file.path(SDIR,"R/my_one_to_one_finder.R"))

require(patchwork)

sample_id=argv[1]

# Validate that sample_id was provided
if (is.na(sample_id) || length(sample_id) == 0) {
  cat("ERROR: No sample ID provided.\n")
  cat("Usage: Rscript analysis01.R <sample_id>\n")
  cat("Example: Rscript analysis01.R GBM_043\n")
  quit(status = 1)
}

# =============================================================================
# DATA LOADING AND VALIDATION
# =============================================================================

cat("Loading data for sample:", sample_id, "\n")
tryCatch({
  dc0=load_cortana_data(sample_id)  # Cortana cell data with exclusion flags
  hh=load_halo_data(sample_id)      # Raw Halo data object
  dh0=extract_geom_data(hh)         # Processed Halo geometry data
}, error = function(e) {
  cat("ERROR: Failed to load data for sample '", sample_id, "':\n", sep="")
  cat(e$message, "\n")
  quit(status = 1)
})

# =============================================================================
# FOV MATCHING AND DATA PREPARATION
# =============================================================================
# 
# Create matched pairs of Cortana and Halo data for each common FOV.
# This ensures spatial analysis is performed only on FOVs present in both datasets.

fovs=intersect(
  dc0 %>% distinct(FOV) %>% pull,
  dh0 %>% distinct(FOV) %>% pull
)

mm=map2(
  (dc0 %>% split(.$FOV))[fovs],
  (dh0 %>% split(.$FOV))[fovs],
  ~ list(Cortana=.x, Halo=.y)
)


# =============================================================================
# MAIN ANALYSIS LOOP: FOV-BY-FOV SPATIAL INTERSECTION ANALYSIS
# =============================================================================
#
# For each FOV, perform spatial intersection analysis between Cortana cells 
# and Halo regions. Generate statistics on overlap patterns and identify 
# one-to-one spatial correspondences for quality assessment.

aggStats=list()

for(ii in seq(mm)) {

  mi=mm[[ii]]

  # Filter out excluded objects for spatial analysis
  dc=mi$Cortana %>% filter(!Exclude)
  dh=mi$Halo %>% filter(!Exclude)

  # Validation: ensure FOV consistency between datasets
  if(dc$FOV[1]!=dh$FOV[1]) {
    rlang::abort("FATAL ERROR::FOV Mismatch")
  }

  fov=dc$FOV[1]
  cat("Processing sample",ii,"fov",fov,"\n")

  # Convert data to spatial objects for geometric analysis
  cat("Creating spatial objects...\n")
  rect_sf <- create_rectangle_sf(dh)
  points_sf <- create_points_sf(dc)

  # Perform bidirectional spatial intersection analysis
  cat("Finding spatial intersections...\n")
  intersections <- find_intersections(rect_sf, points_sf)
  stats <- analyze_intersection_stats(intersections)
  statsTbl=format_stats_table(stats) %>%
    mutate(sample_id=sample_id,fov=fov) %>%
    select(sample_id,fov,everything())

  # Identify one-to-one spatial correspondences using two different methods
  # This validates consistency between spatial analysis approaches
  mapping <- create_intersection_mapping(intersections)
  one_to_one <- find_one_to_one_matches(mapping)
  o2o=find_one_to_one_relationships(intersections)
  if(nrow(one_to_one)!=nrow(o2o)) {
    rlang::abort("FATAL ERROR: one-to-one miss match")
  }

  # Calculate one-to-one match statistics for quality assessment
  o2o_stats=tibble(
      sample_id=sample_id,
      fov=fov,
      intersection_count=NA,
      description="One to one",
      pct_cortana=nrow(one_to_one)/nrow(dc),  # Fraction of Cortana cells with 1:1 matches
      pct_halo=nrow(one_to_one)/nrow(dh)      # Fraction of Halo regions with 1:1 matches
    )

  statsTbl=bind_rows(statsTbl,o2o_stats)
  cat("\n")
  aggStats[[fov]]=statsTbl

  # Generate diagnostic plots for FOVs with poor spatial correspondence
  # Threshold: < 50% one-to-one matches indicates potential alignment issues
  if(o2o_stats$pct_cortana<.5 || o2o_stats$pct_halo<.5) {
    plot_comparisons(mi$Cortana,mi$Halo)
  }

}

# =============================================================================
# RESULTS EXPORT AND SUMMARY STATISTICS
# =============================================================================
#
# Export comprehensive statistics tables for downstream analysis and reporting.
# Two main outputs: overlap statistics (spatial intersection patterns) and 
# exclusion statistics (breakdown of exclusion categories across FOVs).

# Combine all FOV statistics into a single table
aggStats=aggStats %>% bind_rows
fs::dir_create("output/stats")
write_csv(aggStats,cc("output/stats/overlapStats",sample_id,".csv"))

# Generate exclusion statistics summary across all Halo regions
# This provides insight into the distribution of exclusion categories 
# and helps identify potential quality issues in the segmentation
excStats=dh0 %>%
  count(FOV,ExcDesc) %>%                    # Count exclusion types per FOV
  group_by(FOV) %>%
  mutate(pct=n/sum(n)) %>%                  # Calculate percentages within each FOV
  gather(metric,val,n,pct) %>%              # Pivot to long format
  unite(em,metric,ExcDesc,sep="_") %>%      # Create metric_category columns
  spread(em,val,fill=0) %>%                 # Pivot to wide format with 0 fill
  mutate(sample_id=sample_id) %>%
  select(sample_id,fov=FOV,matches("pct"),matches("n_"))
write_csv(excStats,cc("output/stats/excStats",sample_id,".csv"))

