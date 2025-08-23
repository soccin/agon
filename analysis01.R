argv=commandArgs(trailing=T)

source("data_utils.R")
source("plotting_utils.R")
source("spatial_analysis.R")

source("my_one_to_one_finder.R")

require(patchwork)

i=1
sample_id=argv[i]
dc0=load_cortana_data(sample_id) %>% filter(!Exclude)
hh=load_halo_data(sample_id)
dh0=extract_geom_data(hh) %>% filter(!Exclude)


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


ii=1
mi=mm[[ii]]

dc=mi$Cortana
dh=mi$Halo

if(dc$FOV[1]!=dh$FOV[1]) {
  rlang::abort("FATAL ERROR::FOV Mismatch")
}

fov=dc$FOV[1]

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

write_csv(statsTbl,cc("overlapStats",sample_id,fov,".csv"))
