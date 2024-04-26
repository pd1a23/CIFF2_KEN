###Slope

slope<-rast("Z:/Projects/WP000006_CIFF/Raster/KEN/covariates/Slope/MERIT_DEM/v1/ken_slope_merit103_100m_v1.tif")

writeRaster(slope, filename= "raster/unprepared/round1/slope.tif",overwrite=T)
writeRaster(slope, filename= "raster/unprepared/round2/slope.tif",overwrite=T)


### Elevation

elevation<-rast("Z:/Projects/WP000006_CIFF/Raster/KEN/covariates/Elevation/MERIT_DEM/v1/ken_elevation_merit103_100m_v1.tif")

writeRaster(elevation,filename="raster/unprepared/round1/elevation.tif",overwrite=T)
writeRaster(elevation,filename="raster/unprepared/round2/elevation.tif")

### Nightime lights

## DO I use fvf or nvf for 2016 onwards?
##Use fvf

nl_2012<-rast("Z:/Projects/WP000006_CIFF/Raster/KEN/VIIRS/ken_viirs_100m_2012.tif")
nl_2013<-rast("Z:/Projects/WP000006_CIFF/Raster/KEN/VIIRS/ken_viirs_100m_2013.tif")
nl_2014<-rast("Z:/Projects/WP000006_CIFF/Raster/KEN/VIIRS/ken_viirs_100m_2014.tif")

nl_2012_2014<-(nl_2012+nl_2013+nl_2014)/3

writeRaster(nl_2012_2014,filename="raster/unprepared/round1/nighttime_lights.tif")



##Round 2 

nl_2020<-rast("Z:/Projects/WP000006_CIFF/Raster/KEN/covariates/VIIRS/v1/fvf/ken_viirs_fvf_2020_100m_v1.tif")
nl_2021<-rast("Z:/Projects/WP000006_CIFF/Raster/KEN/covariates/VIIRS/v1/fvf/ken_viirs_fvf_2021_100m_v1.tif")
nl_2022<-rast("Z:/Projects/WP000006_CIFF/Raster/KEN/covariates/VIIRS/v1/fvf/ken_viirs_fvf_2022_100m_v1.tif")

nl_2020_2022<-(nl_2020+nl_2021+nl_2022)/3

writeRaster(nl_2020_2022,filename="raster/unprepared/round2/nighttime_lights.tif")
##Precipitation

##Round 1


precip_2012 <-
  rast(
    "Z:/Projects/WP000006_CIFF/Raster/KEN/Amy_covariates/Temp_precip_2012_14/precipmean12.tif"
  )
precip_2013 <-
  rast(
    "Z:/Projects/WP000006_CIFF/Raster/KEN/Amy_covariates/Temp_precip_2012_14/precipmean13.tif"
  )
precip_2014 <-
  rast(
    "Z:/Projects/WP000006_CIFF/Raster/KEN/Amy_covariates/Temp_precip_2012_14/precipmean14.tif"
  )

precip_2012_2014 <- (precip_2012 + precip_2013 + precip_2014) / 3

writeRaster(precip_2012_2014,"raster/unprepared/round1/precipitation.tif",overwrite=T)
##Round 2

precip_2020 <-
  rast(
    "Z:/Projects/WP000006_CIFF/Raster/KEN/covariates/Climate/Precipitation/TerraClim/v1/ken_ppt_2020_yravg_tc_100m_v1.tif"
  )


precip_2021 <-
  rast(
    "Z:/Projects/WP000006_CIFF/Raster/KEN/covariates/Climate/Precipitation/TerraClim/v1/ken_ppt_2021_yravg_tc_100m_v1.tif"
  )

precip_2022 <-
  rast(
    "Z:/Projects/WP000006_CIFF/Raster/KEN/covariates/Climate/Precipitation/TerraClim/v1/ken_ppt_2022_yravg_tc_100m_v1.tif"
  )

precip_2020_2022<-(precip_2020+precip_2021+precip_2022)/3

writeRaster(precip_2020_2022,"raster/unprepared/round2/precipitation.tif")


###Distance to Inland Water 

water_dist<-rast("Z:/Projects/WP000006_CIFF/Raster/KEN/covariates/OSM/v1/ken_waterbodies_dist_osm_2023_100m_v1.tif")
writeRaster(water_dist,"raster/unprepared/round1/water_dist.tif",overwrite=T)
writeRaster(water_dist,"raster/unprepared/round2/water_dist.tif",overwrite=T)



###Travel time to healthcare facilities 

## Should I use walking or driving?

##motorised 

travel_time_health<-rast("Z:/Projects/WP000006_CIFF/Raster/KEN/Amy_covariates/Travel_time_to_health_facilities/KEN_TT_HF_motor.tif")
writeRaster(travel_time_health,"raster/unprepared/round1/travel_time_health.tif")
writeRaster(travel_time_health,"raster/unprepared/round2/travel_time_health.tif")


###Temperature Average

##Round 1

temp_2012 <-
  rast(
    "Z:/Projects/WP000006_CIFF/Raster/KEN/Amy_covariates/Temp_precip_2012_14/tempmean12.tif"
  )

temp_2013 <-
  rast(
    "Z:/Projects/WP000006_CIFF/Raster/KEN/Amy_covariates/Temp_precip_2012_14/tempmean13.tif"
  )

temp_2014 <-
  rast(
    "Z:/Projects/WP000006_CIFF/Raster/KEN/Amy_covariates/Temp_precip_2012_14/tempmean14.tif"
  )


temp_2012_2014<-(temp_2012+temp_2013+temp_2014)/3

writeRaster(temp_2012_2014,"raster/unprepared/round1/temperature.tif")

plot(temp_2012_2014)

##Round 2 

temp_2020<-rast("Z:/Projects/WP000006_CIFF/Raster/KEN/covariates/Climate/Temperature/TerraLST/v1/ken_tavg_2020_tlst_100m_v1.tif")
temp_2021<-rast("Z:/Projects/WP000006_CIFF/Raster/KEN/covariates/Climate/Temperature/TerraLST/v1/ken_tavg_2021_tlst_100m_v1.tif")
temp_2022<-rast("Z:/Projects/WP000006_CIFF/Raster/KEN/covariates/Climate/Temperature/TerraLST/v1/ken_tavg_2022_tlst_100m_v1.tif")

temp_2020_2022<-(temp_2020+temp_2021+temp_2022)/3

writeRaster(temp_2020_2022,"raster/unprepared/round2/temperature.tif")

### Distance to cultivated area 

distance_cultivated_2015<-rast("Z:/Projects/WP000006_CIFF/Raster/KEN/covariates/Land_Cover/ESA/v1/2015/ken_esalc_11_dst_2015_100m_v1.tif")
distance_cultivated_2020<-rast("Z:/Projects/WP000006_CIFF/Raster/KEN/covariates/Land_Cover/ESA/v1/2020/ken_esalc_11_dst_2020_100m_v1.tif")


writeRaster(distance_cultivated_2015,"raster/unprepared/round1/distance_cultivated.tif")
writeRaster(distance_cultivated_2020,"raster/unprepared/round2/distance_cultivated.tif",overwrite=T)
## COuld just use 2015 and 2022- need help with what is what 



###Distance to Coastline 

coastline<-rast("Z:/Projects/WP000006_CIFF/Raster/KEN/covariates/Coastline/v1/Distance/ken_coastline_dst_100m_v1.tif")
writeRaster(coastline,"raster/unprepared/round1/distance_coastline.tif")
writeRaster(coastline,"raster/unprepared/round2/distance_coastline.tif")

##Distance to Roads 

roads_dist<-rast("Z:/Projects/WP000006_CIFF/Raster/KEN/covariates/MS_roads/v1/Distance/ken_ms_roads_dst_100m_v1.tif")
writeRaster(roads_dist,"raster/unprepared/round1/distance_roads.tif")
writeRaster(roads_dist,"raster/unprepared/round2/distance_roads.tif")

##ACLED Distance to conflict 

dist_conflict_2011_2013<-rast("Z:/Projects/WP000006_CIFF/Raster/KEN/Amy_covariates/Distance_to_conflict/KEN_dist_confl_2011_13.tif")
dist_conflict_2019_2021<-rast("Z:/Projects/WP000006_CIFF/Raster/KEN/Amy_covariates/Distance_to_conflict/KEN_dist_confl_2019_21.tif")

writeRaster(dist_conflict_2011_2013,"raster/unprepared/round1/distance_conflict.tif")
writeRaster(dist_conflict_2019_2021,"raster/unprepared/round2/distance_conflict.tif")

##Travel time to urban centres 

travel_to_urban_2015<-rast("Z:/Projects/WP000006_CIFF/Raster/KEN/Amy_covariates/Urban_centres/Outputs/Travel_time/KEN_UC_15_motor.tif")
travel_to_urban_2020<-rast("Z:/Projects/WP000006_CIFF/Raster/KEN/Amy_covariates/Urban_centres/Outputs/Travel_time/KEN_UC_20_motor.tif")


writeRaster(travel_to_urban_2015,"raster/unprepared/round1/traveltime_urban.tif")
writeRaster(travel_to_urban_2020,"raster/unprepared/round2/traveltime_urban.tif")

##Building footprint 

##which one to use?

##Built volume 

built_volume_2015<-rast("Z:/Projects/WP000006_CIFF/Raster/KEN/covariates/BuiltSettlement/Built_volume/ken_built_V_100m_v1_2015.tif")
built_volume_2020<-rast("Z:/Projects/WP000006_CIFF/Raster/KEN/covariates/BuiltSettlement/Built_volume/ken_built_V_100m_v1_2020.tif")

writeRaster(built_volume_2015,"raster/unprepared/round1/built_volume.tif")
writeRaster(built_volume_2020,"raster/unprepared/round2/built_volume.tif")

##WDPA 

distance_protected_2015<-rast("Z:/Projects/WP000006_CIFF/Raster/KEN/covariates/WDPA/v1/cat1/ken_WDPA_pre2015_cat1_dist_100m_v1.tif")
distance_protected_2022<-rast("Z:/Projects/WP000006_CIFF/Raster/KEN/covariates/WDPA/v1/cat1/ken_WDPA_pre2022_cat1_dist_100m_v1.tif")

writeRaster(distance_protected_2015,"raster/unprepared/round1/distance_protected.tif")
writeRaster(distance_protected_2022,"raster/unprepared/round2/distance_protected.tif")

##Which one to use?


