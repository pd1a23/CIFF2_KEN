## function to create covariates at cluster level from raster data


##Function to remove values where inland water percentage is 100% and then 
##resample to the 1km mastergrid

cut_and_resample<-function(raster){
  inland_water_pct<-rast("raster/ken_inland_water_pct_100m_v1.tif")
  master_grid<-rast("raster/WP_mastergrid_L0_2023_v4_1km_v5.tif")
  
  
  values(raster)[values(inland_water_pct)==100]<-NA
  
  
  raster_1km<-terra::resample(raster,master_grid)
  
  return(raster_1km)
  
  
  
  
}



##Function to extract covariates from the separate urban and rural buffers


buffer_extraction <- function(urban_buffer, rural_buffer, raster) {
  
  ##Extract covariates from raster using 2km urban buffers
  urban_covariates <-
    terra::extract(
      raster,
      urban_buffer,
      fun = "mean",
      exact = T,
      na.rm = T,
      bind = T
    )
  
  ##Extract covariates from raster using 5km rural buffers
  rural_covariates <-
    terra::extract(
      raster,
      rural_buffer,
      fun = "mean",
      exact = T,
      na.rm = T,
      bind = T
    )
  
  
  ##Join urban and rural extractions 
  
  covariates <-
    rbind(urban_covariates,
          rural_covariates) %>%
    arrange(DHSCLUST) %>% 
    values()
  


return(covariates)

}


raster_covariates_preparation <-
  function(raster_file_list, cluster_location_file,covariates_filename) {
    
    ##Read in cluster locations from file and remove those with missing coords
    cluster_locations <- vect(cluster_location_file) %>%
      filter(SOURCE != "MIS")
    
    ##Create 2km urban buffer around urban cluster locations
    urban_buffer <-
      cluster_locations %>% filter(URBAN_RURA == "U") %>%
      buffer(width = 2000)
    
    ##Create 5km rural buffer around rural cluster locations
    rural_buffer <-
      cluster_locations %>% filter(URBAN_RURA == "R") %>%
      buffer(width = 5000)
    
    ##Initialise covariate object
    
    full_covariates <- cluster_locations
    
    ##for each raster extract covariates and then cut and resample the raster
    
    for (raster_file in raster_file_list) {
      
      print(raster_file)
      
      raster<-rast(raster_file)
      
      
      covariates<-buffer_extraction(urban_buffer, rural_buffer, raster)
      
      full_covariates <-
        full_covariates %>%
        left_join(covariates)
      
      
      raster_1km<-cut_and_resample(raster)
      raster_1km_file<-raster_file %>% str_remove(".tif") %>% str_c("_prepared.tif")
      
      writeRaster(raster_1km,raster_1km_file)
      
    }
    
    write_csv(values(full_covariates),covariates_filename)
    
  }






raster_file_list<-c("raster/ken_built_V_100m_v1_2015.tif")
cluster_location_file<-"shapes/round2/KEGE71FL.shp"
covariates_filename<-"covariates/round1_test_120324.csv"

raster_covariates_preparation(raster_file_list,cluster_location_file,covariates_filename)



raster_file_list<-c("raster/ken_built_V_100m_v1_2020.tif")
cluster_location_file<-"shapes/round2/KEGE8AFL.shp"
covariates_filename<-"covariates/round2_test_120324.csv"

raster_covariates_preparation(raster_file_list,cluster_location_file,covariates_filename)





