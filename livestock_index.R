## Livestock data

master_grid<-rast("raster/WP_mastergrid_L0_2023_v4_1km_v5.tif")



chickens<-rast("raster/livestock/6_Ch_2015_Aw.tif") %>% crop(master_grid)
cattle<-rast("raster/livestock/6_Ct_2015_Aw.tif") %>% crop(master_grid)
goats<-rast("raster/livestock/6_Gt_2015_Aw.tif") %>% crop(master_grid)
pigs<-rast("raster/livestock/6_Pg_2015_Aw.tif") %>% crop(master_grid)
sheep<-rast("raster/livestock/6_Sh_2015_Aw.tif") %>% crop(master_grid)

which(is.na(values(chickens))!=is.na(values(sheep)))


index<- 
  cbind(
    values(chickens),
    values(cattle),
    values(goats),
    values(pigs),
    values(sheep)
  ) %>% 
  as_tibble %>% filter(!if_any(everything(), is.na))

pca_index<-
  prcomp(
    index,
    scale=T,
    center=T
  )


livestock_index1<-cattle
values(livestock_index1)[!is.na(values(cattle))]<-pca_index$x[,1]
livestock_index1<-
  livestock_index1 %>% 
  resample(master_grid)


livestock_index2<-cattle
values(livestock_index2)[!is.na(values(cattle))]<-pca_index$x[,2]

livestock_index2<-
  livestock_index2 %>% 
  resample(master_grid)


livestock_index3<-cattle
values(livestock_index3)[!is.na(values(cattle))]<-pca_index$x[,3]

livestock_index3<-
  livestock_index3 %>% 
  resample(master_grid)


