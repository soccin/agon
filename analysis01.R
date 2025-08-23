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

# Validate sample data exists before proceeding
cat("Loading data for sample:", sample_id, "\n")
tryCatch({
  dc0=load_cortana_data(sample_id)
  hh=load_halo_data(sample_id)
  dh0=extract_geom_data(hh)
}, error = function(e) {
  cat("ERROR: Failed to load data for sample '", sample_id, "':\n", sep="")
  cat(e$message, "\n")
  quit(status = 1)
})


#
# Get list of common FOV's
#

fovs=intersect(
  dc0 %>% distinct(FOV) %>% pull,
  dh0 %>% distinct(FOV) %>% pull
)

mm=map2(
  (dc0 %>% split(.$FOV))[fovs],
  (dh0 %>% split(.$FOV))[fovs],
  ~ list(Cortana=.x, Halo=.y)
)


aggStats=list()

for(ii in seq(mm)) {

  mi=mm[[ii]]

  dc=mi$Cortana %>% filter(!Exclude)
  dh=mi$Halo %>% filter(!Exclude)

  if(dc$FOV[1]!=dh$FOV[1]) {
    rlang::abort("FATAL ERROR::FOV Mismatch")
  }

  fov=dc$FOV[1]

  cat("Processing sample",ii,"fov",fov,"\n")

  # Step 2: Create spatial objects
  cat("Creating spatial objects...\n")
  rect_sf <- create_rectangle_sf(dh)
  points_sf <- create_points_sf(dc)

  cat("Finding spatial intersections...\n")
  intersections <- find_intersections(rect_sf, points_sf)
  stats <- analyze_intersection_stats(intersections)
  statsTbl=format_stats_table(stats) %>%
    mutate(sample_id=sample_id,fov=fov) %>%
    select(sample_id,fov,everything())

  mapping <- create_intersection_mapping(intersections)
  one_to_one <- find_one_to_one_matches(mapping)
  o2o=find_one_to_one_relationships(intersections)
  if(nrow(one_to_one)!=nrow(o2o)) {
    rlang::abort("FATAL ERROR: one-to-one miss match")
  }

  o2o_stats=tibble(
      sample_id=sample_id,
      fov=fov,
      intersection_count=NA,
      description="One to one",
      pct_cortana=nrow(one_to_one)/nrow(dc),
      pct_halo=nrow(one_to_one)/nrow(dh)
    )

  statsTbl=bind_rows(statsTbl,o2o_stats)

  cat("\n")

  aggStats[[fov]]=statsTbl

  if(o2o_stats$pct_cortana<.5 || o2o_stats$pct_halo<.5) {
    plot_comparisons(mi$Cortana,mi$Halo)
  }

}

aggStats=aggStats %>% bind_rows
fs::dir_create("output/stats")
write_csv(aggStats,cc("output/stats/overlapStats",sample_id,".csv"))

excStats=dh0 %>%
  count(FOV,ExcDesc) %>%
  group_by(FOV) %>%
  mutate(pct=n/sum(n)) %>%
  gather(metric,val,n,pct) %>%
  unite(em,metric,ExcDesc,sep="_") %>%
  spread(em,val,fill=0) %>%
  mutate(sample_id=sample_id) %>%
  select(sample_id,fov=FOV,matches("pct"),matches("n_"))
write_csv(excStats,cc("output/stats/excStats",sample_id,".csv"))

