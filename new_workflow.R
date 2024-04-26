
library(dplyr)
library(tidyr)
library(magrittr)
library(Matrix)
library(tibble)
library(readr)
library(stringr)
library(INLA)
library(terra)
library(tidyterra)
library(matrixStats)
library(magrittr)
library(sf)
library(tidyverse) 


source("utilities/utilities.R")


workflow <- function(indicator_name) {
  indicator_check(indicator_name)
  indicator_fit(indicator_name)
  
    
  
  
}






indicator_check <- function(indicator_name) {
  ##check indicator files for both rounds are in the indicators folder
  
  
  if (any(!(
    paste(indicator_name, c("round1.csv", "round2.csv"), sep = "_") %in%
    list.files(
      path = "indicators/",
      pattern = indicator_name,
      full.names = F
    )
    
  ))) {
    stop(
      "Please check that the indicator name is spelled correctly,
      and that the indicator files for both rounds are in the indicators folder
      and are of the form indicatorname_round1.csv indicatorname_round2.csv"
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
  inla_summary(round1_result[[1]],round1_result[[3]],file_name=indicator_round1)
  
  print("starting round 2 model fit")
  round2_result <- model_fitting(round2_data[[1]], round = "round2")
  inla_summary(round2_result[[1]],round2_result[[3]],file_name=indicator_round2)

   print("starting round 1 prediction")
  round1_prediction <-
    prediction(round1_result[[1]], round1_data[[2]], round1_result[[2]], round =
                 "round1")
  
  write_csv(as.data.frame(round1_prediction) %>% rownames_to_column(var="cell"),
            file = paste("/mainfs/scratch/pd1a23/CIFF2/results/",indicator_round1, "_raw_predictions.csv", sep = ""))
            
print("starting round 2 prediction")
  
  round2_prediction <-
    prediction(round2_result[[1]], round2_data[[2]], round2_result[[2]],round="round2")
  
  write_csv(as.data.frame(round2_prediction) %>% rownames_to_column(var="cell"),
            file = paste("/mainfs/scratch/pd1a23/CIFF2/results/",indicator_round2, "_raw_predictions.csv", sep = ""))
  
  change_prediction <- round2_prediction - round1_prediction
  
  write_csv(as.data.frame(change_prediction),
            file = paste("/mainfs/scratch/pd1a23/CIFF2/results/",indicator_name, "change_raw_predictions.csv", sep = ""))
            
  print("starting district aggregation")
            
  district_aggregation(round1_prediction, change = FALSE, file_name= indicator_round1)
  district_aggregation(round2_prediction, change = FALSE, file_name = indicator_round2)
  district_aggregation(change_prediction,change=TRUE,
                       file_name = paste(indicator_name, "change", sep = "_"))
  
  
  district_aggregation_validation(round1_prediction,round="round1",file_name=indicator_round1)
  district_aggregation_validation(round2_prediction,round="round2",file_name=indicator_round2)
  
  print("starting high res output")
  
  high_res_output(round1_prediction, file_name = indicator_round1)
  high_res_output(round2_prediction, file_name = indicator_round2)
  high_res_output(change_prediction,
                  file_name = paste(indicator_name, "change", sep = "_"))
  
  

  
  
  
}



data_preparation <- function(indicator_round) {
  indicator_file <-
    list.files(path = "indicators/",
               pattern = indicator_round,
               full.names = T)
  
  indicator_data <- read_csv(indicator_file)
  
  if (str_detect(string = indicator_round, pattern = "round1")) {
    covariates <- read_csv("covariates/covariates_round1.csv")
    cluster_locations <- vect("shapes/round1/IAGE71FL.shp")
    
  } else if (str_detect(string = indicator_round, pattern = "round2")) {
    covariates <- read_csv("covariates/covariates_round2.csv")
    cluster_locations <- vect("shapes/round2/IAGE7AFL.shp")
    
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
    left_join(covariates, by = "DHSCLUST") %>%
    
    # Data cleaning - Remove NAs; remove entries that
    # have LON = 0 and LAT = 0 (they are clearly not
    # in India!)
    
    filter(!(LON == 0 & LAT == 0)) %>%
    filter(complete.cases(.)) %>%
    arrange(DHSCLUST)
  
  
  
  # Transform the covariates based on skewness, then scale them to the raster
  ## mean and sd
  
  
  scaled_transformed_intermediate <-
    transform_and_scale(untransformed_data = dat)
  
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

  aspatial_model <-
    covariate_selection(scaled_transformed_cluster_data)
  
  
  covariates_formula <- formula(aspatial_model) %>%
    update(y ~ .)
  
  
  
  temporary_raster <- list.files(path = "covariates/",
                                 pattern = "cows",
                                 full.names = T) %>%
    rast() %>%
    aggregate(fact = 2, na.rm = T)
  
  
  india_boundary <- vect("shapes/gadm41_IND_0.shp") %>%
    project(temporary_raster)
  
  max_edge_length <-
    india_boundary %>%
    geom(df = T) %>%
    summarise(across(.cols = x:y, .fns = c(min, max))) %>%
    summarise(max(abs(x_1 - x_2), abs(y_1 - y_2))) %>%
    divide_by(., 3 * 5) %>%
    as.numeric
  
  outer_boundary_edge <-
    max_edge_length * 5
  
 library(raster) 
  india_boundary_segment <-
    terra::union(india_boundary) %>%
    as(., "Spatial") %>%
    inla.sp2segment()
 detach(package:raster)
  
  india_boundary_points <-
    rbind(
      india_boundary_segment$loc[,1:2],
      cbind(
        scaled_transformed_cluster_data$LON,
        scaled_transformed_cluster_data$LAT
      )
    )
  
  india_boundary_non_convex_hull <- inla.nonconvex.hull(
    points = india_boundary_points,
    convex = max_edge_length / 4,
    concave = max_edge_length / 5,
    resolution = 82
  )
  
  outer_boundary <-
    inla.nonconvex.hull(points = india_boundary_points,
                        convex = outer_boundary_edge,
                        resolution = 50)
  
  mesh <- inla.mesh.2d(
    boundary = list(india_boundary_non_convex_hull, outer_boundary),
    max.edge = c(max_edge_length / 5, 2 * max_edge_length),
    loc = scaled_transformed_cluster_data[, 2:3],
    cutoff = max_edge_length / 5
  )
  
  range0 <-
    0.05 * pmax(abs(ext(india_boundary)[1] - ext(india_boundary)[2]),
                abs(ext(india_boundary)[3] - ext(india_boundary)[4]))
  
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
    cbind(int = 1, scaled_transformed_cluster_data[, 7:ncol(scaled_transformed_cluster_data)])
  
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
    project(temporary_raster)

  ##NEED TO CHECK IF ROUND 1 OR 2
  
  if (round == "round1") {
    district_boundaries <-
      vect("shapes/round1/sdr_subnational_boundaries2.shp") %>%
      project(temporary_raster) %>%
      erase() %>%
      mutate(district_id = 1:nrow(.))
    
  } else if (round == "round2") {
    district_boundaries <-
      vect("shapes/round2/sdr_subnational_boundaries2.shp") %>%
      project(temporary_raster) %>%
      erase() %>%
      mutate(district_id = 1:nrow(.))
    
  }
  
  
  cluster_to_district <-
    cluster_locations2 %>%
    terra::extract(district_boundaries %>% select(district_id),
                   .) %>%
    rename(cluster_id = id.y)
  
  ##put check in here to see if there are any missed clusters
  ## thhen do it
  
  missed_clusters <-
    cluster_locations2[cluster_to_district %>% filter(is.na(district_id)) %$% cluster_id]
  
  
  
  
  if (nrow(missed_clusters) > 0) {
    missed_cluster_nearby_boundaries <-
      nearby(missed_clusters,
             district_boundaries %>% select(district_id),
             centroids = F) %>%
      cbind(cluster_to_district %>% filter(is.na(district_id)) %$% cluster_id,
            .) %>%
      as_tibble %>%
      dplyr::select(cluster_id = V1,
             district_id = k1)
    
    cluster_to_district <-
      cluster_to_district %>%
      as_tibble %>%
      filter(!is.na(district_id)) %>%
      rbind(missed_cluster_nearby_boundaries) %>%
      arrange(cluster_id) %>%
      unique
    
    
  }
  
  
  if (round == "round1") {
    state_boundaries <-
      vect("shapes/round1/sdr_subnational_boundaries.shp") %>%
      project(temporary_raster) %>%
      mutate(state_id = 1:nrow(.))
  } else if (round == "round2") {
    state_boundaries <-
      vect("shapes/round2/sdr_subnational_boundaries.shp") %>%
      project(temporary_raster) %>%
      mutate(state_id = 1:nrow(.))
    
  }
  
  
  
  cluster_to_state <-
    cluster_locations2 %>%
    terra::extract(state_boundaries %>% select(state_id),
                   .) %>%
    rename(cluster_id = id.y)
  
  ##put check in here to see if there are any missed clusters
  ## thhen do it
  
  missed_clusters <-
    cluster_locations2[cluster_to_state %>% filter(is.na(state_id)) %$% cluster_id]
  
  
  
  
  if (nrow(missed_clusters) > 0) {
    missed_cluster_nearby_boundaries <-
      nearby(missed_clusters,
             state_boundaries %>% select(state_id),
             centroids = F) %>%
      cbind(cluster_to_state %>% filter(is.na(state_id)) %$% cluster_id,
            .) %>%
      as_tibble %>%
      dplyr::select(cluster_id = V1,
                    state_id = k1)
    
    cluster_to_state <-
      cluster_to_state %>%
      as_tibble %>%
      filter(!is.na(state_id)) %>%
      rbind(missed_cluster_nearby_boundaries) %>%
      arrange(cluster_id) %>%
      unique
    
    
  }
  
  
  stack_est <- inla.stack(
    A = list(A_est, 1, 1, 1, 1),
    data = list(y = scaled_transformed_cluster_data$count, n = scaled_transformed_cluster_data$total),
    effects = list(
      s = 1:spde$n.spde,
      r = 1:nrow(scaled_transformed_cluster_data),
      d = cluster_to_district$district_id,
      t = cluster_to_state$state_id,
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
        f(d, model = "iid", hyper = iid_hyperprior) + f(t, model = "iid", hyper =
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
  
  spde_res <- inla.spde2.result(inla = inla_model, name = "s", spde = spde)
  
  
  return(list(inla_model, mesh,spde_res))
  
}

# change <- function(inv_lpred_round1, inv_lpred_round2) {
#   inv_lpred_change <- inv_lpred_round2 - inv_lpred_round1
#
#   high_res_output(inv_lpred = inv_lpred_change, file_name = )
#
#   district_aggregation(inv_lpred = inv_lpred_change,
#                        change = T,
#                        file_name = )
#
#   hig
#
#
#
#
#
# }



prediction <- function(model, prediction_data, mesh, round) {
  number_of_samples <- 1000
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
  
  
  temporary_raster <-
    rast(list.files(
      path = "covariates/",
      pattern = "cows",
      full.names = T
    )) %>%
    aggregate(fact = 2, na.rm = T)
  
  
  
  prediction_coordinates <-
    rast(list.files(
      path = "covariates/",
      pattern = "cows",
      full.names = T
    )) %>%
    aggregate(fact = 2, na.rm = T) %>%
    crds(na.rm = F) %>%
    vect(crs = temporary_raster) 
  
 
  
  prediction_covariates_matrix <-
    prediction_data %>%
    cbind(int = 1) %>%
    dplyr::select(model$names.fixed) %>%
    dplyr::relocate(model$names.fixed) %>%
    as.data.frame()
  
  prediction_id <- rowSums(is.na(prediction_covariates_matrix))
  prediction_id_missing <- which(prediction_id > 0)
  prediction_id_nonmiss <- which(prediction_id < 1)
  
  
  ##FIX this naming
  
  
  
  
  if (round == "round1") {
    district_boundaries <-
      vect("shapes/round1/sdr_subnational_boundaries2.shp") %>%
      project(temporary_raster) %>%
      mutate(id = 1:nrow(.))
  } else if (round == "round2") {
    district_boundaries <-
      vect("shapes/round2/sdr_subnational_boundaries2.shp") %>%
      project(temporary_raster) %>%
      mutate(id = 1:nrow(.))
    
    
  }
  
  
  
  
   
  
  cluster_to_district_prediction <-
    terra::extract(temporary_raster, district_boundaries %>% select(id),
                   cells = T) %>% 
    na.omit
  
  
  
  
  
  cluster_to_district_prediction_matrix <-
    sparseMatrix(i = cluster_to_district_prediction$cell,
                 j = cluster_to_district_prediction$ID,
                 x = 1)[prediction_id_nonmiss, ]
  
  
  if (round == "round1") {
    state_boundaries <-
      vect("shapes/round1/sdr_subnational_boundaries.shp") %>%
      project(temporary_raster) %>%
      mutate(id = 1:nrow(.))
  } else if (round == "round2") {
    state_boundaries <-
      vect("shapes/round2/sdr_subnational_boundaries.shp") %>%
      project(temporary_raster) %>%
      mutate(id = 1:nrow(.))
    
  }
  
  
  
  cluster_to_state_prediction <-
    terra::extract(temporary_raster,state_boundaries %>% select(id),
                   cells=T) %>%
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
  
  prediction_covariates_matrix_non_miss<-prediction_covariates_matrix[prediction_id_nonmiss,]
 

  inv_lpred_inter<-as.matrix(
  as.matrix(prediction_covariates_matrix_non_miss) %*% as.matrix(xX) +
    as.matrix(A_prediction) %*% as.matrix(xS) +
    as.matrix(cluster_to_district_prediction_matrix) %*% as.matrix(xD)+
    as.matrix(cluster_to_state_prediction_matrix) %*% as.matrix(xT) #+
    #as.matrix(sIID)
  )


  
   inv_lpred <- gtools::inv.logit(inv_lpred_inter)



  
  return(inv_lpred)
  
  
}



district_aggregation <-
  function(inv_lpred, change = FALSE, file_name) {
        number_of_samples <- ncol(inv_lpred)
    
    
    temporary_raster <- list.files(path = "covariates/",
                                   pattern = "cows",
                                   full.names = T) %>%
      rast() %>%
      aggregate(fact = 2, na.rm = T)
    
    
    
    
    
    id_nonmiss <- which(is.na(values(temporary_raster)))
    
    
    values(temporary_raster)[!is.na(values(temporary_raster))] <- 0
    
    
    
    
    
    pop <- list.files(path = "rasters/denominators/",
                      pattern = "unadj",
                      full.names = T) %>%
      terra::rast() %>%
      terra::aggregate(fact = 2, na.rm = T) %>%
      terra::project(temporary_raster) / 1000
    
    
    
    pop2 <- merge(pop, temporary_raster)
    values(pop2)[is.na(values(temporary_raster))] <- NA
    
  
    target_boundary <-
      vect("shapes/district_boundary.gpkg") %>%
      project(temporary_raster) %>%
      mutate(id = 1:nrow(.))
    
    inv_lpred_df<-as.data.frame(inv_lpred) %>% cbind(values(pop2,na.rm=T)) %>% as_tibble() %>% 
      mutate(
        cell=attr(inv_lpred,"dimnames")[[1]] %>% as.numeric
      )
    colnames(inv_lpred_df) <- c(paste0("mcmc", 1:number_of_samples), "pop","cell")

    

    prediction_raster_to_district1 <-
      terra::extract(
        temporary_raster,
        target_boundary %>% arrange(id) %>%  select(id),
        exact = T,
        cells = T
      )
    prediction_raster_to_district<-
    prediction_raster_to_district1 %>% select(-cows) %>%
      left_join(inv_lpred_df) 
    
 
    library(data.table)
    
    # Convert to data.table
    prediction_raster_to_district_dt <- as.data.table(prediction_raster_to_district)
    
    # Calculate total population per ID
    prediction_raster_to_district_dt[, total_pop := sum(pop, na.rm = TRUE), by = ID]
    
    # Calculate pop_prop and weight
    prediction_raster_to_district_dt[, c("pop_prop", "weight") := .(pop / total_pop, fraction * (pop / total_pop))]
    
    # Define a function for weighted.mean
    weighted.mean.dt <- function(x, w) sum(x * w, na.rm = TRUE) / sum(w, na.rm = TRUE)
    
    # Apply the weighted.mean.dt function across columns mcmc1:mcmc10000
    result_dt <- prediction_raster_to_district_dt[, lapply(.SD, weighted.mean.dt, w = weight), by = ID, .SDcols = patterns("^mcmc")]
    
    # Rename columns
    setnames(result_dt, old = names(result_dt)[2:ncol(result_dt)], new = paste0("mcmc_", 1:(ncol(result_dt)-1)))
    
    
    # Convert to data.table
    raw_agg <- tibble(result_dt)
    
    # Melt the data.table
    melted_dt <- melt(result_dt, id.vars = "ID", measure.vars = patterns("^mcmc"))
    
    # Calculate summary statistics
    agg_dt <- melted_dt[, .(mean = mean(value),
                            sd = sd(value),
                            low_95 = quantile(value, 0.025),
                            up_95 = quantile(value, 0.975),
                            ep=1-ecdf(value)(0)),
                                by = ID]
                        
                 
                        
                      
                        
                        
                        
                        agg<-as_tibble(agg_dt)%>%
                          left_join(target_boundary %>% arrange(id) %>% as.data.frame() %>% select(ID =
                                                                                                      id, State, District,Concatenated))
                                                                                                      
                        if(!change){
                        agg<-agg %>% select(-ep)
                        }
                        
    
    
    
    
    write_csv(raw_agg,
              file = paste0("/mainfs/scratch/pd1a23/CIFF2/results/", file_name, "_raw_district_estimates.csv"))
    
    write_csv(agg,
              file = paste0("/mainfs/scratch/pd1a23/CIFF2/results/", file_name, "_district_estimates.csv"))
  }
  
  
  
  district_aggregation_validation <-
  function(inv_lpred, round, file_name) {
        number_of_samples <- ncol(inv_lpred)
    
    
    temporary_raster <- list.files(path = "covariates/",
                                   pattern = "cows",
                                   full.names = T) %>%
      rast() %>%
      aggregate(fact = 2, na.rm = T)
    
    
    
    
    
    id_nonmiss <- which(is.na(values(temporary_raster)))
    
    
    values(temporary_raster)[!is.na(values(temporary_raster))] <- 0
    
    
    
    
    
    pop <- list.files(path = "rasters/denominators/",
                      pattern = "unadj",
                      full.names = T) %>%
      terra::rast() %>%
      terra::aggregate(fact = 2, na.rm = T) %>%
      terra::project(temporary_raster) / 1000
    
    
    
    pop2 <- merge(pop, temporary_raster)
    values(pop2)[is.na(values(temporary_raster))] <- NA
    
  
    if(round=="round1"){
    target_boundary <-
      vect("shapes/round1/sdr_subnational_boundaries2.shp") %>%
      project(temporary_raster) %>%
      mutate(id = 1:nrow(.))
    }else{
      target_boundary <-
        vect("shapes/round2/sdr_subnational_boundaries2.shp") %>%
        project(temporary_raster) %>%
        mutate(id = 1:nrow(.))
    }
    
    inv_lpred_df<-as.data.frame(inv_lpred) %>% cbind(values(pop2,na.rm=T)) %>% as_tibble() %>% 
      mutate(
        cell=attr(inv_lpred,"dimnames")[[1]] %>% as.numeric
      )
    colnames(inv_lpred_df) <- c(paste0("mcmc", 1:number_of_samples), "pop","cell")

    

    prediction_raster_to_district1 <-
      terra::extract(
        temporary_raster,
        target_boundary %>% arrange(id) %>%  select(id),
        exact = T,
        cells = T
      )
    prediction_raster_to_district<-
    prediction_raster_to_district1 %>% select(-cows) %>%
      left_join(inv_lpred_df) 
    
 
    library(data.table)
    
    # Convert to data.table
    prediction_raster_to_district_dt <- as.data.table(prediction_raster_to_district)
    
    # Calculate total population per ID
    prediction_raster_to_district_dt[, total_pop := sum(pop, na.rm = TRUE), by = ID]
    
    # Calculate pop_prop and weight
    prediction_raster_to_district_dt[, c("pop_prop", "weight") := .(pop / total_pop, fraction * (pop / total_pop))]
    
    # Define a function for weighted.mean
    weighted.mean.dt <- function(x, w) sum(x * w, na.rm = TRUE) / sum(w, na.rm = TRUE)
    
    # Apply the weighted.mean.dt function across columns mcmc1:mcmc10000
    result_dt <- prediction_raster_to_district_dt[, lapply(.SD, weighted.mean.dt, w = weight), by = ID, .SDcols = patterns("^mcmc")]
    
    # Rename columns
    setnames(result_dt, old = names(result_dt)[2:ncol(result_dt)], new = paste0("mcmc_", 1:(ncol(result_dt)-1)))
    
    
    # Convert to data.table
    raw_agg <- tibble(result_dt)
    
    # Melt the data.table
    melted_dt <- melt(result_dt, id.vars = "ID", measure.vars = patterns("^mcmc"))
    
    # Calculate summary statistics
    agg_dt <- melted_dt[, .(mean = mean(value),
                            sd = sd(value),
                            low_95 = quantile(value, 0.025),
                            up_95 = quantile(value, 0.975)),
                                by = ID]
                        
                 
                        
                      
                        
                        
                        
                        agg<-as_tibble(agg_dt)%>%
                          left_join(target_boundary %>% arrange(id) %>% as.data.frame() %>% select(ID =
                                                                                                      id, State= OTHREGNA,District=REGNAME))
                 
    
    
    
    

    
    write_csv(agg,
              file = paste0("/mainfs/scratch/pd1a23/CIFF2/results/", file_name, "_validation_district_estimates.csv"))
  }

high_res_output <- function(inv_lpred, file_name) {

  temporary_raster <-
    rast(list.files(
      path = "covariates/",
      pattern = "cows",
      full.names = T
    )) %>%
    aggregate(fact = 2, na.rm = T)
  
  
  out_raster <-
    rast(
      temporary_raster,
      nlyrs = 5,
      names = c("mean", "sd", "low_95", "upper_95", "median")
    )
  
  
  values(out_raster)[!is.na(values(temporary_raster)),] <-
    cbind(
      rowMeans(inv_lpred,na.rm=T),
      rowSds(inv_lpred,na.rm=T),
      rowQuantiles(inv_lpred, probs = 0.025,na.rm=T),
      rowQuantiles(inv_lpred, probs = 0.975,na.rm=T),
      rowQuantiles(inv_lpred, probs = 0.5,na.rm=T)
    )
    
    out_raster<-
    out_raster %>%
    disagg(fact=2) %>%
    aggregate(fact = 5, na.rm = T)
  
  writeRaster(out_raster,
              filename =paste0("/mainfs/scratch/pd1a23/CIFF2/results/", file_name, "_", c("mean", "sd", "low_95", "upper_95", "median") ,"_high_res_surfaces.tif"),overwrite=T)
  
}




inla_summary <- function(input, input_spde,file_name) {

  # Extract and calculate the summary statistics for the
  # spatial range parameter from the fitted model

  r_mean <- inla.emarginal(function(x) x, input_spde$marginals.range.nominal[[1]])
  r_x2 <- inla.emarginal(function(x) x^2, input_spde$marginals.range.nominal[[1]])
  r_sd <- sqrt(r_x2 - (r_mean^2))
  r_qs <- inla.qmarginal(c(0.025, 0.975), input_spde$marginals.range.nominal[[1]])

  # Extract and calculate the summary statistics for the
  # spatial variance parameter from the fitted model

  v_mean <- inla.emarginal(function(x) x, input_spde$marginals.variance.nominal[[1]])
  v_x2 <- inla.emarginal(function(x) x^2, input_spde$marginals.variance.nominal[[1]])
  v_sd <- sqrt(v_x2-(v_mean^2))
  v_qs <- inla.qmarginal(c(0.025,0.975), input_spde$marginals.variance.nominal[[1]])

  # Extract and calculate the summary statistics for the
  # iid variance parameter from the fitted model

  iid <- inla.tmarginal(function(x) 1/x, input$marginals.hyperpar[[3]])
  iid_summ <- inla.zmarginal(iid, silent = T)

  summ_fix <- summary(input)$fixed[,c(1:3,5)]
  summ_spr <- c(r_mean, r_sd, r_qs)
  summ_spv <- c(v_mean, v_sd, v_qs)
  summ_iid <- c(iid_summ$mean, iid_summ$sd, iid_summ$quant0.025, iid_summ$quant0.975)

  params <- round(rbind(summ_fix, summ_spr, summ_spv, summ_iid),3) %>% as.data.frame()
  write_csv(
  params,
  file=paste0("/mainfs/scratch/pd1a23/CIFF2/results/", file_name,"_model_summary.csv")
  )

}



