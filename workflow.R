library(Matrix)
library(gtools)
library(INLA)
library(terra)
library(tidyterra)
library(matrixStats)
library(magrittr)
library(sf)
library(tidyverse)
library(readxl)

source("utilities.R")

output_folder <- "results/"


workflow <- function(indicator_name) {
  indicator_check(indicator_name)
  
  indicator_fit(indicator_name)
  
  
}


indicator_check <- function(indicator_name) {
  ##check indicator files for both rounds are in the indicators folder
  
  
  if (any(!(
    paste(indicator_name, c("round1.xlsx", "round2.xlsx"), sep = "_") %in%
    list.files(
      path = "indicators/",
      pattern = indicator_name,
      full.names = F
    )
    
  ))) {
    stop(
      "Please check that the indicator name is spelled correctly,
      and that the indicator files for both rounds are in the indicators folder
      and are of the form indicatorname_round1.xlsx indicatorname_round2.xlsx"
    )
    
  }
  
  
  ##check covariates exist
  
  
  if (any(!(
    c("covariates_round1.csv", "covariates_round2.csv") %in%
    list.files(
      path = "covariates/",
      pattern = "covariates",
      full.names = F
    )
    
  ))) {
    stop(
      "Please check that the covariate files for both rounds are in the covariates
      folderand are of the form covariates_round1.csv covariates_round2.csv"
    )
    
  }
  
  ##check the raster covariates exist
  
  covariates_round1 <- read_csv("covariates/covariates_round1.csv")
  covariates_round2 <- read_csv("covariates/covariates_round2.csv")
  
  
  covariate_names <-
    dplyr::union(names(covariates_round1), names(covariates_round2))
  
  
  if (any(!(
    paste(covariate_names[covariate_names != "DHSCLUST"], ".tif", sep = "") %in%
    list.files(
      path = "covariates/",
      pattern = ".tif",
      full.names = F
    )
  ))) {
    stop(
      "Please check that all required raster covariates are in the covariates
      folder and are in tif format"
    )
  }
  
  
}


indicator_fit <- function(indicator_name) {
  indicator_round1 <- paste(indicator_name, "round1", sep = "_")
  indicator_round2 <- paste(indicator_name, "round2", sep = "_")
  
  
  
  round1_data <- data_preparation(indicator_round1)
  round2_data <- data_preparation(indicator_round2)
  print("starting round 1 model fit")
  
  round1_result <- model_fitting(round1_data[[1]], round = "round1")
  inla_summary(round1_result[[1]], round1_result[[3]], file_name = indicator_round1)
  
  print("starting round 2 model fit")
  round2_result <- model_fitting(round2_data[[1]], round = "round2")
  inla_summary(round2_result[[1]], round2_result[[3]], file_name = indicator_round2)
  
  print("starting round 1 prediction")
  round1_prediction <-
    prediction(round1_result[[1]], round1_data[[2]], round1_result[[2]], round =
                 "round1")
  
  write_csv(
    as.data.frame(round1_prediction) %>% rownames_to_column(var = "cell"),
    file = paste(
      output_folder,
      indicator_round1,
      "_raw_predictions.csv",
      sep = ""
    )
  )
  
  print("starting round 2 prediction")
  
  round2_prediction <-
    prediction(round2_result[[1]], round2_data[[2]], round2_result[[2]], round =
                 "round2")
  
  write_csv(
    as.data.frame(round2_prediction) %>% rownames_to_column(var = "cell"),
    file = paste(
      output_folder,
      indicator_round2,
      "_raw_predictions.csv",
      sep = ""
    )
  )
  
  change_prediction <- round2_prediction - round1_prediction
  
  write_csv(
    as.data.frame(change_prediction),
    file = paste(
      output_folder,
      indicator_name,
      "change_raw_predictions.csv",
      sep = ""
    )
  )
  
  print("starting district aggregation")
  
  aggregation_to_boundaries(
    round1_prediction,
    change = FALSE,
    target_boundary_file = "shapes/round1/sdr_subnational_boundaries2.shp",
    file_name = indicator_round1,
    boundary_type = "county",
    raw=T,
    boundary_names = c("DHSREGEN", "REGNAME"),
    round="round1"
  )
  aggregation_to_boundaries(
    round2_prediction,
    change = FALSE,
    target_boundary_file = "shapes/round2/sdr_subnational_boundaries.shp",
    file_name = indicator_round2,
    boundary_type = "county",
    raw = T,
    boundary_names = c("DHSREGEN", "REGNAME"),
    round="round2"
  )
  # aggregation_to_boundaries(
  #   change_prediction,
  #   change = TRUE,
  #   target_boundary_file = "shapes/district_boundary.gpkg",
  #   file_name = paste(indicator_name, "change", sep = "_"),
  #   boundary_type = "district"
  # )
  # 
  ##District aggregation for validation to representative district boundaries
  # aggregation_to_boundaries(
  #   round1_prediction,
  #   change = FALSE,
  #   target_boundary_file = "shapes/round1/sdr_subnational_boundaries2.shp",
  #   file_name = paste(indicator_round1, sep = "_"),
  #   boundary_type = "district_validation",
  #   raw = FALSE
  # )
  # aggregation_to_boundaries(
  #   round2_prediction,
  #   change = FALSE,
  #   target_boundary_file = "shapes/round2/sdr_subnational_boundaries2.shp",
  #   file_name = paste(indicator_round2, sep = "_"),
  #   boundary_type = "district_validation",
  #   raw = FALSE
  # )
  
  ##State Aggregations
  # aggregation_to_boundaries(
  #   round1_prediction,
  #   change = FALSE,
  #   target_boundary_file = "shapes/state_boundary.gpkg",
  #   file_name = indicator_round1,
  #   boundary_type = "state",
  #   raw = FALSE
  # )
  # aggregation_to_boundaries(
  #   round2_prediction,
  #   change = FALSE,
  #   target_boundary_file = "shapes/state_boundary.gpkg",
  #   file_name = indicator_round2,
  #   boundary_type = "state",
  #   raw = FALSE
  # )
  # aggregation_to_boundaries(
  #   change_prediction,
  #   change = TRUE,
  #   target_boundary_file = "shapes/state_boundary.gpkg",
  #   file_name = paste(indicator_name, "change", sep = "_"),
  #   boundary_type = "state",
  #   raw = FALSE
  # )
  
  print("starting high res output")
  
  high_res_output(round1_prediction, file_name = indicator_round1)
  high_res_output(round2_prediction, file_name = indicator_round2)
  high_res_output(change_prediction,
                  file_name = paste(indicator_name, "change", sep = "_"))
  
}



data_preparation <- function(indicator_round) {
  ##Read in the indicator data and associated covariates
  indicator_file <-
    list.files(path = "indicators/",
               pattern = indicator_round,
               full.names = T)
  
  indicator_data <-
    read_xlsx(
      indicator_file,
      skip = 7,
      col_names = c("DHSCLUST", "total", "count", "prop")
    )
  
  if (str_detect(string = indicator_round, pattern = "round1")) {
    covariates <- read_csv("covariates/covariates_round1.csv")
    cluster_locations <- vect("shapes/round1/KEGE71FL.shp")
    round <- "round1"
    
  } else if (str_detect(string = indicator_round, pattern = "round2")) {
    covariates <- read_csv("covariates/covariates_round2.csv")
    cluster_locations <- vect("shapes/round2/KEGE8AFL.shp")
    round <- "round2"
    
  }
  
  
  
  dat <-
    cluster_locations %>%
    as_tibble %>%
    
    # Discard all the additional information given
    # in the shape file (we are only interested
    # in the lat lon coordinates).
    
    select(DHSCLUST,
           LON = LONGNUM,
           LAT = LATNUM) %>%
    
    # Merge indicator with the DHS shape file to get
    # the coordinates then merge with covariates.
    
    right_join(indicator_data, by = "DHSCLUST") %>%
    left_join(covariates %>%
                select(
                  -c(
                    "DHSID",
                    "DHSCC",
                    "DHSYEAR",
                    "CCFIPS",
                    "ADM1FIPS",
                    "ADM1FIPSNA",
                    "ADM1SALBNA",
                    "ADM1SALBCO",
                    "ALT_GPS",
                    "ALT_DEM",
                    "DATUM",
                    "SOURCE"
                  )
                ), by = "DHSCLUST") %>%
    
    # Data cleaning - Remove NAs; remove entries that
    # have LON = 0 and LAT = 0 (they are clearly not
    # in India!)
    
    filter(!(LON == 0 & LAT == 0)) %>%
    filter(complete.cases(.)) %>%
    arrange(DHSCLUST)
  
  
  
  
  # Transform the covariates based on skewness, then scale them to the raster
  ## mean and sd
  
  
  scaled_transformed_intermediate <-
    transform_and_scale(untransformed_data = dat, round = round)
  
  scaled_transformed_cluster_data <-
    scaled_transformed_intermediate[[1]] %>%
    arrange(DHSCLUST)
  
  scaled_transformed_raster_data <-
    scaled_transformed_intermediate[[2]]
  
  return(list(
    scaled_transformed_cluster_data,
    scaled_transformed_raster_data
  ))
}



model_fitting <- function(scaled_transformed_cluster_data, round) {
  mastergrid <- rast("raster/WP_mastergrid_L0_2023_v4_1km_v5.tif")
  
  inland_water_pct <-
    rast("raster/ken_inland_water_pct_100m_v1.tif") %>%
    resample(mastergrid)
  
  
  mastergrid_water_subtract <- mastergrid
  
  values(mastergrid_water_subtract)[values(inland_water_pct) == 100] <-
    NA
  
  
  
  cluster_locations1 <-
    vect("shapes/round1/KEGE71FL.shp") %>% filter(SOURCE != "MIS")
  
  cluster_locations2 <- vect("shapes/round2/KEGE8AFL.shp")
  
  all_cluster_locations <-
    rbind(cluster_locations1, cluster_locations2)
  
  if (round == "round1") {
    cluster_locations <- cluster_locations1
  } else if (round == "round2") {
    cluster_locations <- cluster_locations2
  }
  
  
  country_boundary <-
    as.polygons(mastergrid_water_subtract) %>% disagg()
  country_boundary <- country_boundary[all_cluster_locations]
  
  
  
  max_edge_length <-
    country_boundary %>%
    geom(df = T) %>%
    summarise(across(.cols = x:y, .fns = c(min, max))) %>%
    summarise(max(abs(x_1 - x_2), abs(y_1 - y_2))) %>%
    divide_by(., 3 * 5) %>%
    as.numeric
  
  
  outer_boundary_edge <-
    max_edge_length * 3
  
  library(raster)
  boundary_segment <-
    terra::union(country_boundary) %>%
    as(., "Spatial") %>%
    inla.sp2segment()
  detach(package:raster)


  boundary_segment<-fmesher::fm_as_segm(country_boundary %>% as(.,"Spatial"))
  
  
  
  
  
  country_all_points <-
    rasterize(country_boundary, mastergrid_water_subtract) %>% as.points
  
  # 
  # country_boundary_non_convex_hull <- fmesher::fm_nonconvex_hull_inla(
  #   x = geom(country_all_points)[,3:4],
  #   convex = 0.005,
  #   concave = 0.03,
  #   resolution = 1957
  # )
  
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
  
  
  max.edge <- max(diff(range(geom(
    country_all_points
  )[, 3])), diff(range(geom(
    country_all_points
  )[, 4]))) / 15
  cutoff <- max.edge / 10
  
  
  mesh <- inla.mesh.2d(
    boundary = list(country_boundary_non_convex_hull, outer_boundary),
    max.edge = c(max.edge / 2, 2 * max_edge_length),
    loc = values(cluster_locations)[, 16:17],
    cutoff = cutoff
  )
  aspatial_model <-
    covariate_selection(scaled_transformed_cluster_data)
  
  
  covariates_formula <- formula(aspatial_model) %>%
    update(y ~ .)
 
  
  
  range0 <-
    0.05 * pmax(abs(ext(country_boundary)[1] - ext(country_boundary)[2]),
                abs(ext(country_boundary)[3] - ext(country_boundary)[4]))
  
  # Define the SPDE object. Here we specify the covariance
  # function for the spatial random effect and the priors
  # of the parameters within the covariance function
  
  spde <- inla.spde2.pcmatern(
    mesh = mesh,
    alpha = 1.5,
    prior.range = c(range0, 0.01),
    prior.sigma = c(3, 0.01)
  )
  
  # Create an INLA A object. The INLA A is the projection
  # of the constructed mesh to a sparse matrix
  
  A_est <-
    inla.spde.make.A(loc = as.matrix(scaled_transformed_cluster_data[, 2:3]),
                     mesh = mesh)
  
  scaled_transformed_covariates_and_intercept <-
    cbind(int = 1, scaled_transformed_cluster_data[, 13:ncol(scaled_transformed_cluster_data)])
  
  # Stack everything (the data and the sparse A matrix)
  # together in a format that INLA is happy with
  
  
  cluster_locations2 <-
    scaled_transformed_cluster_data %>%
    dplyr::select(LON,
                  LAT,
                  DHSCLUST) %>%
    arrange(DHSCLUST) %>%
    as.data.frame %>%
    vect(geom = c("LON", "LAT"),
         crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
    project(mastergrid)
  
  ##NEED TO CHECK IF ROUND 1 OR 2
  
  # if (round == "round1") {
  #   district_boundaries <-
  #     vect("shapes/round1/sdr_subnational_boundaries2.shp") %>%
  #     project(mastergrid) %>%
  #     erase() %>%
  #     mutate(district_id = 1:nrow(.))
  #
  # } else if (round == "round2") {
  #   district_boundaries <-
  #     vect("shapes/round2/sdr_subnational_boundaries.shp") %>%
  #     project(mastergrid) %>%
  #     erase() %>%
  #     mutate(district_id = 1:nrow(.))
  #
  # }
  
  
  
  
  
  
  stack_est <- inla.stack(
    A = list(A_est, 1, 1, 1, 1),
    data = list(y = scaled_transformed_cluster_data$count, n = scaled_transformed_cluster_data$total),
    effects = list(
      s = 1:spde$n.spde,
      r = 1:nrow(scaled_transformed_cluster_data),
      d = scaled_transformed_cluster_data$ADM1DHS,
      t = scaled_transformed_cluster_data$DHSREGCO,
      scaled_transformed_covariates_and_intercept
    ),
    tag = "est"
  )
  
  
  
  stacks <- inla.stack(stack_est)
  
  iid_hyperprior <-
    list(theta = list(prior = "loggamma", param = c(2, 1)))
  
  
  full_formula <-
    update(
      covariates_formula,
      . ~ -1 + int + . + f(s, model = spde) +
        f(r, model = "iid", hyper = iid_hyperprior) +
        f(d, model = "iid", hyper = iid_hyperprior)
      + f(t, model = "iid", hyper =
            iid_hyperprior)
    )
  
  env <- environment()
  
  
  
  inla_model <- inla(
    formula = full_formula,
    family = "binomial",
    data = inla.stack.data(stacks),
    Ntrials = n,
    control.predictor = list(
      compute = T,
      link = 1,
      A = inla.stack.A(stacks)
    ),
    control.compute = list(config = T, waic = T),
    verbose = F,
    .parent.frame = env
  )
  
  
  spde_res <-
    inla.spde2.result(inla = inla_model,
                      name = "s",
                      spde = spde)
  
  
  return(list(inla_model, mesh, spde_res))
  
}



prediction <- function(model, prediction_data, mesh, round) {
  number_of_samples <- 100
  model_contents <- model$misc$configs$contents
  posterior_samples <-
    inla.posterior.sample(number_of_samples, model)
  
  id_s <- which(model_contents$tag == "s")
  id_r <- which(model_contents$tag == "r")
  id_d <- which(model_contents$tag == "d")
  id_t <- which(model_contents$tag == "t")
  id_x <- which(model_contents$tag == "int")
  
  index_S <-
    model_contents$start[id_s] - 1 + (1:model_contents$length[id_s])
  index_R <-
    model_contents$start[id_r] - 1 + (1:model_contents$length[id_r])
  index_D <-
    model_contents$start[id_d] - 1 + (1:model_contents$length[id_d])
  index_T <-
    model_contents$start[id_t] - 1 + (1:model_contents$length[id_t])
  index_X <-
    model_contents$start[id_x] - 1 + (1:ncol(model$model.matrix))
  
  
  # Create a prediction matrix with the values from the 5x5km
  # covariate rasters (make sure they are in the same order as
  # the fitted model).
  
  
  mastergrid <- rast("raster/WP_mastergrid_L0_2023_v4_1km_v5.tif")
  
  inland_water_pct <-
    rast("raster/ken_inland_water_pct_100m_v1.tif") %>%
    resample(mastergrid, method = "average")
  
  
  mastergrid_water_subtract <- mastergrid
  
  values(mastergrid_water_subtract)[values(inland_water_pct) == 100] <-
    NA
  
  
  if (round == "round1") {
    district_boundaries <-
      vect("shapes/round1/sdr_subnational_boundaries2.shp") %>%
      project(mastergrid) %>%
      aggregate(by="REGCODE") %>% 
      mutate(id = 1:nrow(.))
  } else if (round == "round2") {
    district_boundaries <-
      vect("shapes/round2/sdr_subnational_boundaries.shp") %>%
      aggregate(by="REGCODE") %>% 
      project(mastergrid) %>%
      mutate(id = 1:nrow(.))
    
    
  }
  
  cluster_locations1 <-
    vect("shapes/round1/KEGE71FL.shp") %>% filter(SOURCE != "MIS")
  
  cluster_locations2 <- vect("shapes/round2/KEGE8AFL.shp")
  
  all_cluster_locations <-
    rbind(cluster_locations1, cluster_locations2)
  
  district_boundaries <-
    disagg(district_boundaries)[all_cluster_locations]
  
  # mastergrid_water_subtract_district_crop <-
  #   mastergrid_water_subtract %>% crop(district_boundaries, mask = T)
  #
  # writeRaster(mastergrid_water_subtract_district_crop,"raster/mastergrid_crop.tif",overwrite=T)
  
  mastergrid_water_subtract_district_crop <-
    rast("raster/mastergrid_crop.tif")
  
  prediction_covariates_matrix <-
    prediction_data %>%
    cbind(int = 1) %>%
    dplyr::select(model$names.fixed) %>%
    dplyr::relocate(model$names.fixed) %>%
    as.data.frame()
  
  # prediction_covariates_rast <- rast(mastergrid, nlyrs = ncol(prediction_covariates_matrix))
  # values(prediction_covariates_rast) <- prediction_covariates_matrix
  # values(prediction_covariates_rast)[values(inland_water_pct) == 100] <- NA
  # prediction_covariates_rast <- prediction_covariates_rast %>% crop(district_boundaries, mask = T)
  
  
  
  
  prediction_coordinates <-
    mastergrid_water_subtract_district_crop %>%
    crds(na.rm = F) %>%
    vect(crs = mastergrid_water_subtract_district_crop)
  
  
  
  
  ##CHANGE THIS NAMING
  #
  # prediction_covariates_matrix<-values(prediction_covariates_rast)
  
  
  
  prediction_id <- rowSums(is.na(prediction_covariates_matrix))
  prediction_id_missing <- which(prediction_id > 0)
  prediction_id_nonmiss <- which(prediction_id < 1)
  
  
  ##FIX this naming
  
  
  
  
  
  
  cluster_to_district_prediction <-
    terra::extract(
      mastergrid_water_subtract_district_crop,
      district_boundaries %>% select(REGCODE) %>% arrange(REGCODE),
      cells = T,
      touches = T
    ) %>% na.omit
  
  

  
  
  cluster_to_district_prediction_matrix <-
    sparseMatrix(i = cluster_to_district_prediction$cell,
                 j = cluster_to_district_prediction$ID,
                 x = 1)[prediction_id_nonmiss, ]
  
  
  state_boundaries <-
    vect("shapes/round1/sdr_subnational_boundaries.shp") %>%
    project(mastergrid) %>%
    mutate(id = 1:nrow(.))
  
  
  
  
  state_boundaries <-
    disagg(state_boundaries)[all_cluster_locations] %>% 
    aggregate(by="REGCODE")
  
  
  cluster_to_state_prediction <-
    terra::extract(
      mastergrid_water_subtract_district_crop,
      state_boundaries %>% select(REGCODE) %>% arrange(REGCODE),
      cells = T,
      touches = T
    ) %>%
    na.omit
  
  cluster_to_state_prediction_matrix <-
    sparseMatrix(i = cluster_to_state_prediction$cell,
                 j = cluster_to_state_prediction$ID,
                 x = 1)[prediction_id_nonmiss,]
  
  
  
  # Create a prediction (INLA) A matrix based on the coordinates
  # of the 5x5km covariate rasters and extract the prediction
  # matrix values (without the coordinates) for our calculation
  # later.
  
  A_prediction <-
    inla.spde.make.A(loc = as.matrix(crds(prediction_coordinates[prediction_id_nonmiss, ])),
                     mesh = mesh)
  
  
  
  
  
  
  # Create empty matrices to hold the values from the posterior sampling
  # object. Extract the values from the posterior sampling object into
  # the empty matrices.
  
  posterior_latent <-
    lapply(posterior_samples, function(x)
      x %$% latent) %>% unlist %>% matrix(ncol = number_of_samples)
  posterior_hyper_paramter <-
    lapply(posterior_samples, function(x)
      x %$% hyperpar) %>% unlist %>% matrix(ncol = number_of_samples)
  posterior_sigma <- posterior_hyper_paramter[3,] ^ (-0.5)
  sIID <-
    sapply(posterior_sigma, function(x)
      rnorm(nrow(prediction_covariates_matrix[prediction_id_nonmiss,]), sd = x))
  
  # Further subset the values extracted from the posterior sampling
  # object according to the IDs created above (to separate out
  # values for the spatial random effect, the fixed effect, etc.)
  
  xS <- posterior_latent[index_S,]
  xD <-
    cbind(model$summary.random$d$ID, posterior_latent[index_D,]) %>% as_tibble() %>% right_join(tibble(V1 =
                                                                                                         1:nrow(district_boundaries))) %>%
    mutate(across(everything(), ~ replace_na(.x, 0))) %>% select(-V1) %>% as.matrix
  xT <-
    cbind(model$summary.random$t$ID, posterior_latent[index_T,]) %>% as_tibble() %>% right_join(tibble(V1 =
                                                                                                         1:nrow(state_boundaries))) %>%
    mutate(across(everything(), ~ replace_na(.x, 0))) %>% select(-V1) %>% as.matrix
  
  xX <- posterior_latent[index_X,]
  
  prediction_covariates_matrix_non_miss <-
    prediction_covariates_matrix[prediction_id_nonmiss,]
  
  
  inv_lpred_inter <- as.matrix(
    as.matrix(prediction_covariates_matrix_non_miss) %*% as.matrix(xX) +
      as.matrix(A_prediction) %*% as.matrix(xS) +
      as.matrix(cluster_to_district_prediction_matrix) %*% as.matrix(xD) +
      as.matrix(cluster_to_state_prediction_matrix) %*% as.matrix(xT) +
      as.matrix(sIID)
  )
  
  
  
  inv_lpred <- gtools::inv.logit(inv_lpred_inter)
  
  
  
  
  return(inv_lpred)
}



aggregation_to_boundaries <-
  function(inv_lpred,
           change = FALSE,
           target_boundary_file,
           file_name,
           boundary_type,
           raw = T,
           boundary_names = c("State", "District", "Concatenated"),
           round) {
    number_of_samples <- ncol(inv_lpred)
    
    
    mastergrid_water_subtract_district_crop <-
      mastergrid_water_subtract_district_crop <-
      rast("raster/mastergrid_crop.tif")
    
    
    id_nonmiss <-
      which(is.na(values(
        mastergrid_water_subtract_district_crop
      )))
    
    
    values(mastergrid_water_subtract_district_crop)[!is.na(values(mastergrid_water_subtract_district_crop))] <-
      0
    
    
    if (round == "round1") {
      pop <- rast("raster/round1/ken_ppp_2014_1km_Aggregated_UNadj.tif") %>%
        terra::resample(
          mastergrid_water_subtract_district_crop,
          method = "average",
          threads = T
        ) / 1000
    } else if (round == "round2") {
      pop <- rast("raster/round2/ken_ppp_2020_1km_Aggregated_UNadj.tif") %>%
        terra::resample(
          mastergrid_water_subtract_district_crop,
          method = "average",
          threads = T
        ) / 1000
    }
    
    pop2 <- merge(pop, mastergrid_water_subtract_district_crop)
    
    
    values(pop2)[is.na(values(mastergrid_water_subtract_district_crop))] <-
      NA
    
    
    target_boundary <-
      vect(target_boundary_file) %>%
      project(mastergrid_water_subtract_district_crop) %>%
      mutate(id = 1:nrow(.))
    
    
    inv_lpred_df <-
      as.data.frame(inv_lpred) %>% cbind(values(pop2, na.rm = T)) %>% as_tibble() %>%
      mutate(cell = attr(inv_lpred, "dimnames")[[1]] %>% as.numeric)
    colnames(inv_lpred_df) <-
      c(paste0("mcmc", 1:number_of_samples), "pop", "cell")
    
    
    prediction_raster_to_district1 <-
      terra::extract(
        mastergrid_water_subtract_district_crop,
        target_boundary %>% arrange(id) %>%  select(id),
        exact = T,
        cells = T
      )
    
    
    prediction_raster_to_district <-
      prediction_raster_to_district1 %>% select(-WP_mastergrid_L0_2023_v4) %>%
      left_join(inv_lpred_df)
    
    
    library(data.table)
    
    # Convert to data.table
    prediction_raster_to_district_dt <-
      as.data.table(prediction_raster_to_district)
    
    # Calculate total population per ID
    prediction_raster_to_district_dt[, total_pop := sum(pop, na.rm = TRUE), by = ID]
    
    # Calculate pop_prop and weight
    prediction_raster_to_district_dt[, c("pop_prop", "weight") := .(pop / total_pop, fraction * (pop / total_pop))]
    
    # Define a function for weighted.mean
    weighted.mean.dt <-
      function(x, w)
        sum(x * w, na.rm = TRUE) / sum(w, na.rm = TRUE)
    
    # Apply the weighted.mean.dt function across columns mcmc1:mcmc10000
    result_dt <-
      prediction_raster_to_district_dt[, lapply(.SD, weighted.mean.dt, w = weight), by = ID, .SDcols = patterns("^mcmc")]
    
    # Rename columns
    setnames(result_dt,
             old = names(result_dt)[2:ncol(result_dt)],
             new = paste0("mcmc_", 1:(ncol(result_dt) - 1)))
    
    
    # Convert to data.table
    raw_agg <- tibble(result_dt)
    
    # Melt the data.table
    melted_dt <-
      melt(result_dt,
           id.vars = "ID",
           measure.vars = patterns("^mcmc"))
    
    # Calculate summary statistics
    agg_dt <- melted_dt[, .(
      mean = mean(value),
      sd = sd(value),
      low_95 = quantile(value, 0.025),
      up_95 = quantile(value, 0.975),
      ep = 1 - ecdf(value)(0)
    ),
    by = ID]
    
    agg <- as_tibble(agg_dt) %>%
      left_join(target_boundary %>% arrange(id) %>% as.data.frame() %>% select(all_of((
        c("ID" = "id", boundary_names)
      ))))
    
    if (!change) {
      agg <- agg %>% select(-ep)
    }
    
    
    if (raw) {
      write_csv(
        raw_agg,
        file = paste0(
          output_folder,
          file_name,
          "_raw_",
          boundary_type,
          "_estimates.csv"
        )
      )
    }
    
    write_csv(agg,
              file = paste0(
                output_folder,
                file_name,
                "_",
                boundary_type,
                "_estimates.csv"
              ))
  }


high_res_output <- function(inv_lpred, file_name) {
  
  mastergrid_water_subtract_district_crop <-
    mastergrid_water_subtract_district_crop <-
    rast("raster/mastergrid_crop.tif")
  
  
  out_raster <-
    rast(
      mastergrid_water_subtract_district_crop,
      nlyrs = 5,
      names = c("mean", "sd", "low_95", "upper_95", "median")
    )
  
  
  values(out_raster)[!is.na(values(mastergrid_water_subtract_district_crop)),] <-
    cbind(
      rowMeans(inv_lpred, na.rm = T),
      rowSds(inv_lpred, na.rm = T),
      rowQuantiles(inv_lpred, probs = 0.025, na.rm = T),
      rowQuantiles(inv_lpred, probs = 0.975, na.rm = T),
      rowQuantiles(inv_lpred, probs = 0.5, na.rm = T)
    )
  
  out_raster <-
    out_raster %>%
    aggregate(fact = 5, na.rm = T)
  
  writeRaster(
    out_raster,
    filename = paste0(
      output_folder,
      file_name,
      "_",
      c("mean", "sd", "low_95", "upper_95", "median") ,
      "_high_res_surfaces.tif"
    ),
    overwrite = T
  )
  
}




inla_summary <- function(input, input_spde, file_name) {
  # Extract and calculate the summary statistics for the
  # spatial range parameter from the fitted model
  
  r_mean <-
    inla.emarginal(function(x)
      x, input_spde$marginals.range.nominal[[1]])
  r_x2 <-
    inla.emarginal(function(x)
      x ^ 2, input_spde$marginals.range.nominal[[1]])
  r_sd <- sqrt(r_x2 - (r_mean ^ 2))
  r_qs <-
    inla.qmarginal(c(0.025, 0.975), input_spde$marginals.range.nominal[[1]])
  
  # Extract and calculate the summary statistics for the
  # spatial variance parameter from the fitted model
  
  v_mean <-
    inla.emarginal(function(x)
      x, input_spde$marginals.variance.nominal[[1]])
  v_x2 <-
    inla.emarginal(function(x)
      x ^ 2, input_spde$marginals.variance.nominal[[1]])
  v_sd <- sqrt(v_x2 - (v_mean ^ 2))
  v_qs <-
    inla.qmarginal(c(0.025, 0.975), input_spde$marginals.variance.nominal[[1]])
  
  # Extract and calculate the summary statistics for the
  # iid variance parameter from the fitted model
  
  iid <-
    inla.tmarginal(function(x)
      1 / x, input$marginals.hyperpar[[3]])
  iid_summ <- inla.zmarginal(iid, silent = T)
  
  summ_fix <- summary(input)$fixed[, c(1:3, 5)]
  summ_spr <- c(r_mean, r_sd, r_qs)
  summ_spv <- c(v_mean, v_sd, v_qs)
  summ_iid <-
    c(iid_summ$mean,
      iid_summ$sd,
      iid_summ$quant0.025,
      iid_summ$quant0.975)
  
  params <-
    round(rbind(summ_fix, summ_spr, summ_spv, summ_iid), 3) %>% as.data.frame()
  write_csv(params,
            file = paste0(output_folder,
                          file_name,
                          "_model_summary.csv"))
  
}
