library(Matrix)
library(gtools)
library(INLA)
library(terra)
library(tidyterra)
library(matrixStats)
library(magrittr)
library(sf)
library(tidyverse)


mastergrid<- rast("raster/WP_mastergrid_L0_2023_v4_1km_v5.tif")

inland_water_pct<-rast("raster/ken_inland_water_pct_100m_v1.tif") %>% 
  resample(mastergrid)


mastergrid_water_subtract<-mastergrid

values(mastergrid_water_subtract)[values(inland_water_pct)==100]<-NA



cluster_locations1 <- vect("shapes/round1/KEGE71FL.shp") %>% filter(SOURCE!="MIS")

cluster_locations2<- vect("shapes/round2/KEGE8AFL.shp")

all_cluster_locations<-rbind(cluster_locations1,cluster_locations2)


country_boundary<-as.polygons(mastergrid_water_subtract) %>% disagg()
country_boundary<-country_boundary[all_cluster_locations] 



max_edge_length <-
  country_boundary %>%
  geom(df = T) %>%
  summarise(across(.cols = x:y, .fns = c(min, max))) %>%
  summarise(max(abs(x_1 - x_2), abs(y_1 - y_2))) %>%
  divide_by(., 3 * 5) %>%
  as.numeric


outer_boundary_edge <-
  max_edge_length * 3

# library(raster)
# boundary_segment <-
#   terra::union(country_boundary) %>%
#   as(., "Spatial") %>%
#   inla.sp2segment()
# detach(package:raster)
# 
# 
# boundary_segment<-fmesher::fm_as_segm(country_boundary %>% as(.,"Spatial"))





country_all_points <-rasterize(country_boundary,mastergrid_water_subtract) %>% as.points


# country_boundary_non_convex_hull <- fmesher::fm_nonconvex_hull_inla(
#   x = geom(country_all_points)[,3:4],
#   convex = 0.005,
#   concave = 0.03,
#   resolution = 1957
# )

load("shapes/country_boundary_non_convex_hull.Rdata")
# outer_boundary <-
#   inla.nonconvex.hull(points = geom(country_all_points)[,3:4],
#                       convex = outer_boundary_edge,
#                       resolution = 50)

load("shapes/outer_boundary.Rdata")


max.edge = max(diff(range(geom(country_all_points)[,3])), diff(range(geom(country_all_points)[,4])))/15
cutoff = max.edge/10


mesh <- inla.mesh.2d(
  boundary = list(country_boundary_non_convex_hull, outer_boundary),
  max.edge = c(max.edge/2, 2 * max_edge_length),
  loc = values(cluster_locations)[, 16:17],
  cutoff = cutoff
)


