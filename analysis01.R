argv=commandArgs(trailing=T)

source("data_utils.R")
source("plotting_utils.R")
source("spatial_analysis.R")

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

halt("DDDDD")

ph=plot_rectangles(dh) + labs(subtitle="Halo",title=paste(sample_id,"FOV:",fov))
pc=plot_cells_basic(dc) + labs(subtitle="Cortana",title=paste(sample_id,"FOV:",fov))

p1=ph + pc

Xcm=dh %>% pull(X) %>% mean
Ycm=dh %>% pull(Y) %>% mean

s=.5;
p2=plot_cells_and_rectangles(dh,dc,xlim=s*16*c(-100,100)+Xcm,ylim=s*9*c(-100,100)+Ycm)
s=.2;
p3=plot_cells_and_rectangles(dh,dc,xlim=s*16*c(-100,100)+Xcm,ylim=s*9*c(-100,100)+Ycm)

# Step 2: Create spatial objects
cat("Creating spatial objects...\n")
rect_sf <- create_rectangle_sf(dh)
points_sf <- create_points_sf(dc)

cat("Finding spatial intersections...\n")
intersections <- find_intersections(rect_sf, points_sf)
stats <- analyze_intersection_stats(intersections)
statsTbl=format_stats_table(stats) %>% mutate(SampleID=sample_id) %>% select(SampleID,everything())

write_csv(statsTbl,cc("overlapStats",sample_id,".csv"))

pdf(file=cc("pltsHaloVsCortana",sample_id,".pdf"),width=11,height=8.5)
print(ph+pc)
print(p2)
print(p3)
dev.off()
