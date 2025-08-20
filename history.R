require(tidyverse)
argv=commandArgs(trailing=T)
argv[1]
read_csv(argv[1])
argv
read_csv("mpIF/GBM/10_samples/results/cell_table_with_exclusions/GBM_043___cell_table_size_normalized_final.csv")
dd=read_csv("mpIF/GBM/10_samples/results/cell_table_with_exclusions/GBM_043___cell_table_size_normalized_final.csv")
dd %>% select(label:cell_size,area:ncol(dd))
dd %>% select(label:cell_size,area:ncol(dd)) %>% select(-matches("_nuclear"))
dd %>% select(label:cell_size,area:ncol(dd)) %>% select(-matches("_nuclear")) %>% str
dx=dd %>% select(label:cell_size,area:ncol(dd)) %>% select(-matches("_nuclear")) 
ggplot(dx,aes(Xc,Yc)) + geom_point()
ggplot(dx,aes(Xc,Yc)) + geom_point() + theme_light()
ggplot(dx,aes(Xc,Yc)) + geom_point() + theme_light() + coord_fixed()
ggplot(dx,aes(Xc,Yc)) + geom_point(alpha=.2) + theme_light() + coord_fixed()
x11()
ggplot(dx,aes(Xc,Yc)) + geom_point(alpha=.2) + theme_light() + coord_fixed()
scan("haloRDAs","")
scan("haloRDAs","") %>% grep("")
de=read_csv(argv[1])
de
de %>% select(-matches("_nuclear"))
de %>% select(-matches("_nuclear")) %>% select(cell_size,label:ncol(de))
de %>% select(-matches("_nuclear")) %>% select(cell_size,label:mask_type)
dg
dx
de %>% select(-matches("_nuclear")) %>% select(cell_size,label:mask_type)
scan("haloRDAs","") %>% grep("43")
scan("haloRDAs","") %>% grep("43",.)
scan("haloRDAs","") %>% grep("GBM_043",.,value=T)
hh=readRDS(scan("haloRDAs","") %>% grep("GBM_043",.,value=T))
dh=hh$geom.data
dh
dh %>% select(FOV,matches("^[XY]"))
dh %>% select(UUID,FOV,matches("^[XY]"))
di=dh %>% select(UUID,FOV,matches("^[XY]"))
ggplot(di,aes(Xc,Yc)) + geom_point(alpha=.2) + theme_light() + coord_fixed()
ggplot(di,aes(x0,y0)) + geom_point(alpha=.2) + theme_light() + coord_fixed()
ggplot(dx,aes(Xc,Yc)) + geom_point(alpha=.2) + theme_light() + coord_fixed()
x11()
ggplot(di,aes(x0,y0)) + geom_point(alpha=.2) + theme_light() + coord_fixed()
x11()
ggplot(dx,aes(Xc,Yc)) + geom_point(alpha=.2) + theme_light() + coord_fixed()
di
ggplot(dx,aes(Xc,Yc)) + geom_point(alpha=.2) + theme_light() + coord_fixed()
x1()
x11()
ggplot(di,aes(X,Y)) + geom_point(alpha=.2) + theme_light() + coord_fixed()
di
str(di)
str(di%>%select(UUID,XMin,XMax,YMin,YMax))
di
di %>% ggplot(aes(xmin=XMin,xmax=XMax,ymin=YMin,ymax=YMax)) + geom_rec()
di %>% ggplot(aes(xmin=XMin,xmax=XMax,ymin=YMin,ymax=YMax)) + geom_rect()
library(plotly)
p=di %>% ggplot(aes(xmin=XMin,xmax=XMax,ymin=YMin,ymax=YMax)) + geom_rect()
ggplotly(p)
p+coord_cartesian(xlim=c(2000,2100),ylim=c(2000,2100)))
p+coord_cartesian(xlim=c(2000,2100),ylim=c(2000,2100))
di %>% ggplot(aes(xmin=XMin,xmax=XMax,ymin=YMin,ymax=YMax)) + geom_rect(fill=NA) + coord_cartesian(xlim=c(2000,2100),ylim=c(2000,2100))
di %>% ggplot(aes(xmin=XMin,xmax=XMax,ymin=YMin,ymax=YMax)) + geom_rect(fill=NA,color="black") + coord_cartesian(xlim=c(2000,2100),ylim=c(2000,2100))
ggplot() + geom_rect(data=di,aes(xmin=XMin,xmax=XMax,ymin=YMin,ymax=YMax),fill=NA,color="black") + coord_cartesian(xlim=c(2000,2100),ylim=c(2000,2100))
ggplot() + theme_minimal() + geom_rect(data=di,aes(xmin=XMin,xmax=XMax,ymin=YMin,ymax=YMax),fill=NA,color="black") + coord_cartesian(xlim=c(2000,2100),ylim=c(2000,2100)) 
di
de
dd
de
de
de
dg
ls()
dx
ggplot() + theme_minimal() + geom_rect(data=di,aes(xmin=XMin,xmax=XMax,ymin=YMin,ymax=YMax),fill=NA,color="black") + coord_cartesian(xlim=c(2000,2100),ylim=c(2000,2100)) + geom_point(data=dx,aes(Xc,Yx))
ggplot() + theme_minimal() + geom_rect(data=di,aes(xmin=XMin,xmax=XMax,ymin=YMin,ymax=YMax),fill=NA,color="black") + coord_cartesian(xlim=c(2000,2100),ylim=c(2000,2100)) + geom_point(data=dx,aes(Xc,Yc))
ggplot() + theme_minimal() + geom_rect(data=di,aes(xmin=XMin,xmax=XMax,ymin=YMin,ymax=YMax),fill=NA,color="black") + coord_cartesian(xlim=c(2000,2500),ylim=c(2000,2500)) + geom_point(data=dx,aes(Xc,Yc))
ggplot() + theme_minimal() + geom_rect(data=di,aes(xmin=XMin,xmax=XMax,ymin=YMin,ymax=YMax),fill=NA,color="black") + geom_point(data=dx,aes(Xc,Yc)) + coord_cartesian(xlim=c(2000,2500),ylim=c(2000,2500)) 
ggplot() + theme_minimal() + geom_rect(data=di,aes(xmin=XMin,xmax=XMax,ymin=YMin,ymax=YMax),fill=NA,color="black") + geom_point(data=dx,aes(Xc,Yc)) + coord_cartesian(xlim=c(2000,3000),ylim=c(2000,3000)) 
ggplot() + theme_minimal() + geom_rect(data=di,aes(xmin=XMin,xmax=XMax,ymin=YMin,ymax=YMax),fill=NA,color="black") + geom_point(data=dx,aes(Xc,Yc)) + coord_cartesian(xlim=c(2000,3000),ylim=c(2000,3500)) 
ggplot() + theme_minimal() + geom_rect(data=di,aes(xmin=XMin,xmax=XMax,ymin=YMin,ymax=YMax),fill=NA,color="black") + geom_point(data=dx,aes(Xc,Yc)) + coor_fixed(xlim=c(2000,3000),ylim=c(2000,3500)) 
ggplot() + theme_minimal() + geom_rect(data=di,aes(xmin=XMin,xmax=XMax,ymin=YMin,ymax=YMax),fill=NA,color="black") + geom_point(data=dx,aes(Xc,Yc)) + coor_fix(xlim=c(2000,3000),ylim=c(2000,3500)) 
ggplot() + theme_minimal() + geom_rect(data=di,aes(xmin=XMin,xmax=XMax,ymin=YMin,ymax=YMax),fill=NA,color="black") + geom_point(data=dx,aes(Xc,Yc)) + coord_fixed(xlim=c(2000,3000),ylim=c(2000,3500)) 
ggplot() + theme_minimal() + geom_rect(data=di,aes(xmin=XMin,xmax=XMax,ymin=YMin,ymax=YMax),fill=NA,color="black") + geom_point(data=dx,aes(Xc,Yc)) + coord_fixed(xlim=c(2000,3500),ylim=c(2000,3000)) 
library(sf)
di
dr=di
dr_sf <- dr %>%
  rowwise() %>%
  do({
    # Create polygon from rectangle bounds
    poly <- st_polygon(list(matrix(c(
      .$XMin, .$YMin,
      .$XMax, .$YMin, 
      .$XMax, .$YMax,
      .$XMin, .$YMax,
      .$XMin, .$YMin
    ), ncol = 2, byrow = TRUE)))
    
    data.frame(UUID = .$UUID, geometry = st_sfc(poly))
  }) %>%
  st_sf()
dr_sf
# Convert points to sf points
dp_sf <- st_as_sf(dp, coords = c("X", "Y"))
dp_sf <- st_as_sf(dx, coords = c("Xc", "Yc"))
dp_sf
dp_sf %>% select(UUID,geometry)
dp_sf %>% select(label,geometry)
dp_sf=dp_sf %>% select(label,geometry)
intersections <- st_intersects(dr_sf, dp_sf)
intersections
dr_sf %>%
  mutate(
    point_indices = intersections,
    point_count = lengths(intersections)
  )
results <- dr_sf %>%
  mutate(
    point_indices = intersections,
    point_count = lengths(intersections)
  ) %>%
  st_drop_geometry() 
rectangle_points <- map2_dfr(dr$UUID, intersections, function(rect_uuid, point_idx) {
  if(length(point_idx) > 0) {
    data.frame(
      rectangle_UUID = rect_uuid,
      point_UUID = dp$UUID[point_idx],
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      rectangle_UUID = rect_uuid,
      point_UUID = character(0),
      stringsAsFactors = FALSE
    )
  }
})
intersections
class(intersections)
intersect[[1]]
intersect[1]
?st_intersects
st_intersect(dp_sf,dr_sf)
st_intersects(dp_sf,dr_sf)
?st_intersects
library(data.table)
?st_intersects
lst=st_intersects(dp_sf,dr_sf)
len(lst)
dim(dp_sf)
dim(dr_sf)
dim(dp_sf)
lst[[1]]
lst[[2]]
map(lst,~.)
map_vec(lst,~.)
map(lst,~.)
map(lst,~len(.))
map_vec(lst,~len(.))
table(map_vec(lst,~len(.)))
lstT=st_intersects(dr_sf,dp_sf)
table(map_vec(lstT,~len(.)))
tt=table(map_vec(lstT,~len(.)))
tt/sum(tt)
100*tt/sum(tt)
prettyNum(100*tt/sum(tt))
tt=table(map_vec(lst,~len(.)))
prettyNum(100*tt/sum(tt))
lst
unlist(lst)
map(lst,as_tibble)
lst[1:100]
lst[1:10]
lst[1:20]
lst[1:100]
ll=lst[1:100]
ll %>% data.frame
map(ll,\(x)c(x))
map(ll,\(x)paste0(x))
map(ll,\(x)paste0(x,collapse=";"))
map_vec(ll,\(x)paste0(x,collapse=";"))
map_vec(lst,\(x)paste0(x,collapse=";"))
tibble(seq(lst),map_vec(lst,\(x)paste0(x,collapse=";")))
tibble(Cortana=seq(lst),Halo=map_vec(lst,\(x)paste0(x,collapse=";")))
dim(dp)
dp
dx
tibble(Cortana=seq(lst),Halo=map_vec(lst,\(x)paste0(x,collapse=";")))
tibble(Halo=seq(lstT),Cortana=map_vec(lst,\(x)paste0(x,collapse=";")))
tibble(Halo=seq(lstT),Cortana=map_vec(lstT,\(x)paste0(x,collapse=";")))
ch=tibble(Cortana=seq(lst),Halo=map_vec(lst,\(x)paste0(x,collapse=";")))
hc=tibble(Halo=seq(lstT),Cortana=map_vec(lstT,\(x)paste0(x,collapse=";")))
ch %>% filter(Cortana!="")
ch %>% filter(Halo!="")
ch %>% filter(Halo!="" & !grepl(";",Halo))
h1=ch %>% filter(Halo!="" & !grepl(";",Halo)) %>% pull(Halo)
hc
hc %>% filter(Halo %in% h1)
hc %>% filter(Halo %in% h1) %>% filter(Cortana!="" & !grepl(";",Cortana))
ch %>% filter(Halo!="" & !grepl(";",Halo))
dx
dx %>% filter(label==45)
di %>% filter(row_number()==386)
ch
ch=tibble(Cortana=seq(lst),Halo=map_vec(lst,\(x)paste0(x,collapse=";")))
ch %>% mutate(lH=len(Halo))

ch=tibble(Cortana=seq(lst),Halo=map_vec(lst,\(x)paste0(x,collapse=";")))
ch=tibble(Cortana=seq(lst),Halo=map_vec(lst,\(x)list(x)))
ch
ch %>% mutate(nH=len(Halo))
ch %>% rowwise %>% mutate(nH=len(Halo))
ch %>% rowwise %>% mutate(nH=len(Halo)) %>% arrange(desc(nH))
ch %>% rowwise %>% mutate(nH=len(Halo)) %>% arrange(desc(nH)) %>% unnest(Halo)
hc=tibble(Halo=seq(lstT),Cortana=map_vec(lstT,\(x)list(x)))
hc
hc %>% unnest(Cortana)
full_join(hc,ch,by="Halo")
full_join(hc,ch%>%unnest(Halo),by="Halo")
full_join(hc,ch%>%unnest(Halo),by="Halo") %>% filter(!is.na(Cortana.y))
full_join(hc,ch%>%unnest(Halo),by="Halo") %>% filter(!is.na(Cortana.y)) %>% group_by(Cortana.y) %>% filter(n()>1)
full_join(hc,ch%>%unnest(Halo),by="Halo") %>% filter(!is.na(Cortana.y)) %>% group_by(Cortana.y) %>% filter(n()==1)
full_join(hc,ch%>%unnest(Halo),by="Halo") %>% filter(!is.na(Cortana.y)) %>% group_by(Cortana.y) %>% filter(n()==1) %>% group_by(Halo) %>% filter(n()==1)
full_join(hc,ch%>%unnest(Halo),by="Halo") %>% filter(!is.na(Cortana.y)) %>% group_by(Cortana.y) %>% filter(n()==1) %>% group_by(Halo) %>% filter(n()==1) %>% unnest(Cortana.x)
dim(dp)
dim(dx)
dim(di)
15787 / c(20009,17852)
dp
di
