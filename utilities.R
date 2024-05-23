## UTILITIES

require(tidyverse)
require(magrittr)
require(terra)
require(sf)



## PETER CHECK
delta <- function(covariate) {
  covariate2 <- na.omit(covariate)
  delta <- min(covariate2[covariate2 > 0]) / 2
  return(delta)
}

## PETER CHECK
variable_transformation_check <-
  function(outcome, covariate, power, grid) {
    if (power == 0) {
      power_covariate = log(covariate + delta(grid))
    }
    else if (power < 0) {
      # If the power is negative add a negative to ensure that the
      # ordering (and hence the skewness direction) is maintained
      power_covariate = -((covariate + delta(grid)) ^ power)
      
      
    }
    else{
      power_covariate = covariate ^ power
    }
    
    skew = e1071::skewness(power_covariate, na.rm = T)
    
    data = tibble(
      total = outcome$total,
      count = outcome$count,
      power_covariate = power_covariate
    ) %>%
      na.omit()
    
    mod = glm(
      formula = cbind(count, total - count) ~ power_covariate,
      family = "binomial",
      data = data
    )
    
    return(c(skew, logLik(mod)))
    
  }


fn_omit <- function(id, mcol, data) {
  f1 <- paste("(count/total) ~", as.character(mcol$Var1[id]))
  f2 <- paste("(count/total) ~", as.character(mcol$Var2[id]))
  
  f1_mod <- glm(
    formula = f1,
    weights = total,
    family = binomial,
    data = data
  )
  
  f2_mod <- glm(
    formula = f2,
    weights = total,
    family = binomial,
    data = data
  )
  
  f1_BIC <- BIC(f1_mod)
  f2_BIC <- BIC(f2_mod)
  
  ifelse(f1_BIC < f2_BIC,
         return(as.character(mcol$Var2[id])),
         return(as.character(mcol$Var1[id])))
  
}

covariate_selection <- function(input) {
  cormat <- cor(input[, 14:ncol(input)])
  cormat[upper.tri(cormat, diag = T)] <- 0
  
  # Check for multicollinearity from the Pearson's correlation
  # coefficient matrix by observing pairs that return > 0.8
  
  mcol <- reshape2::melt(cormat) %>%
    filter(abs(value) > 0.8)
  
  # Compare the BIC of the GLM fitted with the individual covariates
  # from the problematic pairs. Omit the covariate from the model
  # that gives a larger BIC value
  
  omit <- unique(sapply(1:nrow(mcol), function(x)
    fn_omit(x, mcol, input)))
  
  fdat <- input %>% select(4,5,14:ncol(input)) %>% dplyr::select(-all_of(omit))
  
  # Use the step function in the backward direction to select the
  # optimal subset of covariates. Object f_sel is the formula we
  # will use for the INLA part
  
  mod <- glm(
    formula = count / total ~ .,
    weights = total,
    family = "binomial",
    data =fdat
  ) %>%
    step(.,
         direct = "backward",
         k = log(nrow(fdat)),
         trace = 0)
  
  return(mod)
  
}


variable_transformation <- function(outcome, covariate, grid) {
  # Define power ladders
  
  power_list_down = c(1, 1 / 2, 1 / 3, 0, -1 / 2, -1, -2, -3, -4, -5)
  power_list_up = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
  
  
  skew = e1071::skewness(covariate, na.rm = T)
  
  # Assume that skew between 1 and -1 is ok!
  # If the skew is between 1 and -1 do nothing
  # Else work up/down the power ladder
  
  if (dplyr::between(skew,-1, 1)) {
    return(list(covariate, 1))
  } else if (skew > 1) {
    power_list = power_list_down
  } else {
    power_list = power_list_up
  }
  
  skew_j = skew
  ll_list = c()
  j = 0
  
  # While skew_j > 1 for skew > 1 or skew < -1 for skew < -1
  
  while ((abs(skew_j) > 1) & (sign(skew_j) == sign(skew))) {
    j = j + 1
    if (j > length(power_list)) {
      print("power list insufficient")
      break
    }
    
    # Run the transformation for the jth element of the power list
    
    vt = variable_transformation_check(outcome, covariate, power_list[j], grid)
    skew_j = vt[1]
    ll = vt[2]
    
    # Add the log likelihood for the transformation
    
    ll_list = c(ll_list, ll)
    
  }
  
  # Check which gives the maximum log likelihood (equivalent
  # to AIC or BIC; but they are all single covariate models
  # so it doesn't matter)
  
  max_ll = which.max(ll_list)
  
  # Pick this transformation from the power list and add the
  # transformed covariate to the intitalised tibble
  
  transformation = power_list[max_ll]
  
  # Special case for log transformation
  
  if (transformation == 0) {
    transformed_covariate <- log(covariate + delta(covariate))
  } else if (transformation < 0) {
    transformed_covariate <-
      -((covariate + delta(covariate)) ^ transformation)
  } else {
    transformed_covariate <- covariate ^ transformation
  }
  
  # Add the transformation to the list
  
  # transformation_list[i] = round(transformation, 2)
  return(list(transformed_covariate, transformation))
  
}


transform_and_scale <- function(untransformed_data,round) {
  # Input:
  # Data frame consisting with covariate names appended
  # with the corresponding power transformation number
  
  scaled_transformed_cluster_data <- untransformed_data
  scaled_transformed_raster_data <- list()
  
  
  covariates <- untransformed_data %>% dplyr::select(-c(1:13))
  scaled_covs <- covariates
  
  for (ii in 1:ncol(covariates)) {
    print(ii)
    covariate <- covariates %>% pull(names(covariates)[[ii]])
    covariate_name <- names(covariates)[[ii]]
    
    grid <- list.files(path = paste0("raster/",round,"/"),
                       pattern = covariate_name,
                       full.names = T) %>%
      rast() %>%
      values()
    
    
    # Account for any covariates that have negative values- it just
    # shifts them to have a minimum of 0
    
    # if (any(grid < 0, na.rm = T)) {
    #   covariate = covariate + abs(min(grid, na.rm = T))
    #   grid = grid + abs(min(grid, na.rm = T))
    # }
    # 
    # transformation_inter <-
    #   variable_transformation(untransformed_data %>% dplyr::select(total, count),
    #                           covariate,
    #                           grid)
    # transformed_covariate <- transformation_inter[[1]]
    # transformation <- transformation_inter[[2]]
    
    
    # Apply power transformation according to the
    # appended power transformation number
    
    # if (is.na(transformation)) {
    #   transformed_grid <- grid
    #   
    # } else if (transformation == 0) {
    #   transformed_grid <- log(grid + delta(grid))
    #   
    # } else if (transformation < 0) {
    #   transformed_grid <-
    #     -((grid + delta(grid)) ^ transformation)
    #   
    # } else {
    #   transformed_grid <- grid ^ transformation
    #   
    # }
    
    scaled_grid <-
      scale(grid)
    
    scaling_attributes <- attributes(scaled_grid)
    
    scaled_covariate <-
      (covariate - scaling_attributes$`scaled:center`) / scaling_attributes$`scaled:scale`
    
    
    
    scaled_transformed_cluster_data[[covariate_name]] <-
      scaled_covariate
    scaled_transformed_raster_data[[covariate_name]] <-
      as.vector(scaled_grid)
    
    
  }
  
  # Output:
  # A list of #aggregated, transformed and scaled raster
  # values of the covariates
  
  scaled_transformed_cluster_data
  
  return(list(
    scaled_transformed_cluster_data,
    as_tibble(scaled_transformed_raster_data)
  ))
  
}


transform_and_scale <- function(untransformed_data,round) {
  # Input:
  # Data frame consisting with covariate names appended
  # with the corresponding power transformation number
  
  scaled_transformed_cluster_data <- untransformed_data
  scaled_transformed_raster_data <- list()
  
  
  covariates <- untransformed_data %>% dplyr::select(-c(1:13))
  scaled_covs <- covariates
  
  for (ii in 1:ncol(covariates)) {
    print(ii)
    covariate <- covariates %>% pull(names(covariates)[[ii]])
    covariate_name <- names(covariates)[[ii]]
    
    grid <- list.files(path = paste0("raster/",round,"/"),
                       pattern = covariate_name,
                       full.names = T) %>%
      rast() %>%
      values()
    
    
    # Account for any covariates that have negative values- it just
    # shifts them to have a minimum of 0
    
    # if (any(grid < 0, na.rm = T)) {
    #   covariate = covariate + abs(min(grid, na.rm = T))
    #   grid = grid + abs(min(grid, na.rm = T))
    # }
    # 
    # transformation_inter <-
    #   variable_transformation(untransformed_data %>% dplyr::select(total, count),
    #                           covariate,
    #                           grid)
    # transformed_covariate <- transformation_inter[[1]]
    # transformation <- transformation_inter[[2]]
    
    
    # Apply power transformation according to the
    # appended power transformation number
    
    # if (is.na(transformation)) {
    #   transformed_grid <- grid
    #   
    # } else if (transformation == 0) {
    #   transformed_grid <- log(grid + delta(grid))
    #   
    # } else if (transformation < 0) {
    #   transformed_grid <-
    #     -((grid + delta(grid)) ^ transformation)
    #   
    # } else {
    #   transformed_grid <- grid ^ transformation
    #   
    # }
    
    scaled_grid <-
      scale(grid)
    
    scaling_attributes <- attributes(scaled_grid)
    
    scaled_covariate <-
      (covariate - scaling_attributes$`scaled:center`) / scaling_attributes$`scaled:scale`
    
    
    
    scaled_transformed_cluster_data[[covariate_name]] <-
      scaled_covariate
    scaled_transformed_raster_data[[covariate_name]] <-
      as.vector(scaled_grid)
    
    
  }
  
  # Output:
  # A list of #aggregated, transformed and scaled raster
  # values of the covariates
  
  scaled_transformed_cluster_data
  
  return(list(
    scaled_transformed_cluster_data,
    as_tibble(scaled_transformed_raster_data)
  ))
  
}



