argv=commandArgs(trailing=T)

source("data_utils.R")
source("plotting_utils.R")
source("spatial_analysis.R")

require(patchwork)

i=1
sample_id=argv[i]
dc0=read_cell_table(sample_id) %>% clean_cell_data
hh=load_halo_data(sample_id)
dh0=extract_geom_data(hh)




ph=plot_rectangles(dh) + labs(subtitle="Halo",title=sample_id)
pc=plot_cells_basic(dc) + labs(subtitle="Cortana",title=sample_id)

p1=ph + pc

s=1;
p2=plot_cells_and_rectangles(dh,dc,xlim=s*16*c(-100,100)+3000,ylim=s*9*c(-100,100)+4000)
s=.25;
p3=plot_cells_and_rectangles(dh,dc,xlim=s*16*c(-100,100)+3000,ylim=s*9*c(-100,100)+4000)

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
