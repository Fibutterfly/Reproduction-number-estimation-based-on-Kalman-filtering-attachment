library(dplyr)
library(KFAS)
require(stats)
#' KalmanFilter Class Definition
#'
#' A class for implementing Kalman Filtering with specific parameters and options.
#' 
#' @slot dataset A data.frame holding the dataset for filtering.
#' @slot output_folder Character. The folder path for saving output files.
#' @slot output_filename Character. The filename for the output of the Kalman filter analysis.
#' @slot min_T Numeric. The minimum threshold for T (e.g., time or temperature).
#' @slot gamma Numeric. The rate of recovery or transition parameter in the model.
#' @slot min_signal_to_noise Numeric. The minimum signal-to-noise ratio considered.
#' @slot max_signal_to_noise Numeric. The maximum signal-to-noise ratio considered.
#' @slot days_infection Numeric. The number of days of infection to model.
#' @slot n_start_values_grid Numeric. The number of starting values for grid optimization.
#' @slot maxiter Numeric. Maximum number of iterations for the optimization algorithm.
#' @slot optim A data.frame holding the optimization results.
#' @slot alpha1 Numeric. Lower confidence interval bound.
#' @slot alpha2 Numeric. Upper confidence interval bound.
#' 
#' @prototype The prototype sets default values for each slot.
#' 
#' @examples
#' # Create an instance of KalmanFilter with default values
#' kf <- new("KalmanFilter")

setClass("KalmanFilter",
         slots = c(
           dataset = "data.frame",
           output_folder = "character",
           output_filename = "character",
           min_T = "numeric",
           gamma = "numeric",
           min_signal_to_noise = "numeric",
           max_signal_to_noise = "numeric",
           days_infection = "numeric",
           n_start_values_grid = "numeric",
           maxiter = "numeric",
           optim = "data.frame",
           alpha1 = "numeric",
           alpha2 = "numeric"
         ),
         prototype = c(
           dataset = data.frame(),
           output_folder = "./data/estimate",
           output_filename= "Kalman_filter.csv",
           min_T = 20,
           gamma = 1/7,
           min_signal_to_noise = 0.01,
           max_signal_to_noise = 0.25,
           days_infection = 7,
           n_start_values_grid = 5,
           maxiter = 200,
           optim = data.frame(),
           alpha1 = 0.05, 
           alpha2 = 0.35
         )
         
)

#' Impose Minimum Time Series Observations on KalmanFilter Object
#'
#' This method for the `KalmanFilter` class filters the dataset within the `KalmanFilter` object,
#' ensuring that only time series with a minimum number of observations specified by the `KalmanFilter`
#' object's `@min_T` attribute are retained for analysis. This filtering is based on the number of
#' non-NA observations for a specific time series, identified by a dynamically generated column name
#' based on the `@days_infection` attribute.
#'
#' @param x An object of class `KalmanFilter`. The `KalmanFilter` object containing the dataset
#'   to be filtered, along with the attributes `@dataset`, `@days_infection`, and `@min_T` that
#'   specify the dataset to filter, the suffix for generating the column name of interest, and the
#'   minimum number of observations required, respectively.
#' @return Returns the modified `KalmanFilter` object, where the `@dataset` attribute has been
#'   updated to include only those time series that meet or exceed the minimum number of observations
#'   specified by the `@min_T` attribute. The filtering process respects groupings by `Country/Region`
#'   within the dataset.
#' @export
#' @examples
#' # Assuming `kf` is an existing `KalmanFilter` object with properly set attributes
#' # and a dataset that includes a `Country/Region` column and various `gr_infected_*` columns:
#' kf_filtered <- impose_minimum_timeseries_observations(kf)
#' # Now `kf_filtered@dataset` will only contain rows belonging to `Country/Region` groups with
#' # a sufficient number of non-NA observations in the `gr_infected_*` column specified by
#' # `kf@days_infection`.
#' @method impose_minimum_timeseries_observations KalmanFilter
setGeneric("impose_minimum_timeseries_observations",
          function(x) standardGeneric("impose_minimum_timeseries_observations"))
setMethod("impose_minimum_timeseries_observations","KalmanFilter", function(x) {
  column_name <- sprintf("gr_infected_%s", x@days_infection)
  df_temp <- x@dataset %>%
    group_by(`Country/Region`) %>%
    summarise(Count = sum(!is.na(.data[[column_name]]))) %>%
    ungroup() %>%
    rename(no_obs = Count)
  wrk_df <- x@dataset %>% left_join(df_temp, by = "Country/Region")
  
  filtered_df <- wrk_df %>%
    filter("no_obs" >= x@min_T)
  x@dataset <- filtered_df
  return(x)
})


#' Estimate Reproduction Number R
#'
#' This method for the `KalmanFilter` class estimates the effective reproduction number, R, for infectious diseases from time series data. It employs a local level model via state space modeling techniques, optimizing the model parameters to fit the observed data points.
#'
#' @param x An object of class `KalmanFilter`. This object should contain attributes such as `@days_infection`, `@n_start_values_grid`, `@maxiter`, and `@gamma` which are utilized in the estimation process.
#' @param dataset A data frame or similar object containing the time series data. The dataset should include a column named dynamically based on the `@days_infection` attribute of the `KalmanFilter` object, representing the growth rate of infections.
#' @return Returns a list containing the estimated effective reproduction number `R` and its standard error `se_R`, along with additional model diagnostics including the variance of the observation and state equations (`sigma2_irregular` and `sigma2_level`), the signal-to-noise ratio (`signal_to_noise`), the scaling factor `gamma`, and a convergence flag (`flag`) indicating whether the optimization process converged successfully.
#' @export
#' @examples
#' # Assuming `kf` is an existing `KalmanFilter` object with the necessary attributes
#' # and `data` is a dataset with a column for the growth rate of infections:
#' estimate <- estimate_R_KF(kf, data)
#' print(estimate$R)
#' print(estimate$se_R)
#' @method estimate_R_KF KalmanFilter
setGeneric("estimate_R_KF",
          function(x,dataset) standardGeneric("estimate_R_KF"))
setMethod("estimate_R_KF","KalmanFilter", function(x,dataset) {
  # Create the model specification for a local level model
  col_name <- sprintf("gr_infected_%s", x@days_infection)
  y <- unlist(dataset[[col_name]])
  initial_Q <- 1e-4
  initial_H <- 1e-2 
  # model_spec <- SSModel(y ~ SSMtrend(degree = 1,  Q = list(matrix(initial_Q))),
                        # H = matrix(initial_H))
  # model_spec <- SSModel(y ~ SSMtrend(degree = 1))
  if(x@n_start_values_grid > 0){
    opt_res <- list(
      obj_value = c(),
      start_val_1 = c(),
      start_val_2 = c()
    )
    start_vals_grid <- seq(0.01, 2.0, length.out = x@n_start_values_grid)*var(y,na.rm=TRUE)
    for(start_val_1 in start_vals_grid){
      for(start_val_2 in start_vals_grid){
        mod_spec <- SSModel(y ~ SSMtrend(degree = 1, Q = matrix(start_val_1)),
                            H = matrix(start_val_2))
        # mod_spec <- SSModel(y ~ SSMarima(ar=1,Q=1, stationary = FALSE))
        # print("mod_spec")
        # print(mod_spec)
        # tryCatch({
          mod_fit <- fitSSM(model = mod_spec, inits = c(start_val_1, start_val_2), 
                 method = "BFGS", control = list(maxit = x@maxiter))
          
          # print(mod_fit)
          final_value <- mod_fit$optim.out$value
          opt_res$obj_value <- c(opt_res$obj_value,final_value)
          opt_res$start_val_1 = c(opt_res$start_val_1,start_val_1)
          opt_res$start_val_2 = c(opt_res$start_val_2,start_val_2)
        # }, error = function(e) {
          # Return NA values if the model fails to fit
          # print(start_val_1)
        #  opt_res$obj_value <- c(opt_res$obj_value,NA)
        #  opt_res$start_val_1 = c(opt_res$start_val_1,start_val_1)
        #  opt_res$start_val_2 = c(opt_res$start_val_2,start_val_2)
        #})

        #print(mod_fit)
      }
    }
    # print(opt_res)
    df_opt_res <- data.frame(opt_res)
    df_opt_res <- df_opt_res[order(df_opt_res$obj_value),]
    mod_spec <- SSModel(y ~ SSMtrend(degree = 1, Q = matrix(NA)),
                        H = matrix(NA))
    res_ll <- fitSSM(model = mod_spec, inits = c(df_opt_res$start_val_1[1], df_opt_res$start_val_2[1]), 
                     method = "BFGS", control = list(maxit = x@maxiter))
    #print(max(df_opt_res$obj_value))
    #print(min(df_opt_res$obj_value))
    #print(-logLik(min(df_opt_res$obj_value)))
  }
  else{
    mod_spec <- SSModel(y ~ SSMtrend(degree = 1, Q = matrix(NA)),
                        H = matrix(NA))
    res_ll <- fitSSM(model = mod_spec, inits = c(initial_Q, initial_H), 
                     method = "BFGS", control = list(maxit = x@maxiter))
  }
  smoothed <- KFS(res_ll$model,simplify = FALSE)
  est_val <- smoothed$alphahat
  est_var <- smoothed$V
  R <- 1+1/x@gamma*est_val
  se_R <- (1/x@gamma * est_var)^0.5
  se_R <- matrix(se_R, nrow = length(se_R), ncol = 1)
  return(list(
    R=R,
    se_R=se_R,
    sigma2_irregular=smoothed$model$H, #Ez itt egyáltalán nem biztos,
    sigma2_level=smoothed$model$Q,
    signal_to_noise=smoothed$model$Q  /smoothed$model$H ,
    gamma = x@gamma,
    flag = res_ll$optim.out$convergence
  ))
})

#' Make R Estimations for a KalmanFilter Object
#'
#' This method processes a `KalmanFilter` object's dataset to estimate the effective reproduction number, R, and its standard error, along with several model diagnostics, for each unique `Country/Region` within the dataset.
#'
#' @param KF A `KalmanFilter` object containing the dataset to be processed. The dataset should have a column `Country/Region` to identify the geographical unit for which the R value is estimated. The `KalmanFilter` object must also be appropriately initialized with model parameters suitable for estimating R.
#' @return A modified version of the `KalmanFilter` object's dataset with additional columns for the estimated effective reproduction number (`R`), its standard error (`se_R`), variance of the observation equation (`sigma2_irregular`), variance of the state equation (`sigma2_level`), signal-to-noise ratio (`signal_to_noise`), and a flag indicating the success of the estimation process (`flag`) for each `Country/Region`.
#' @export
#' @examples
#' # Assuming `kf` is an existing `KalmanFilter` object properly initialized and containing a dataset with the required structure:
#' updated_dataset <- make_R_estimations(kf)
#' head(updated_dataset)
#' @method make_R_estimations KalmanFilter
setGeneric("make_R_estimations",
           function(KF) standardGeneric("make_R_estimations"))
setMethod("make_R_estimations","KalmanFilter", function(KF) {
  wrk_set <- KF@dataset
  wrk_set$R <- NA
  wrk_set$se_R <- NA
  wrk_set$sigma2_irregular <- NA
  wrk_set$sigma2_level <- NA
  wrk_set$signal_to_noise <- NA
  wrk_set$flag <- NA
  countries <- unique(wrk_set$`Country/Region`)
  
  
  for (act_country in countries) {
    mask = wrk_set$`Country/Region` == act_country
    filtered_data <- wrk_set[mask,]
    out_data <- estimate_R_KF(KF,filtered_data)
    wrk_set$R[mask] <- out_data$R
    wrk_set$se_R[mask] <- out_data$se_R
    wrk_set$sigma2_irregular[mask] <- out_data$sigma2_irregular
    wrk_set$sigma2_level[mask] <- out_data$sigma2_level
    wrk_set$signal_to_noise[mask] <- out_data$signal_to_noise
    wrk_set$flag[mask] <- out_data$flag
  }
  return(wrk_set)
})

#' Filter R Estimations for Reliability and Signal-to-Noise Ratio
#'
#' This method filters the dataset within a `KalmanFilter` object, retaining only those estimations of the effective reproduction number (R) that are deemed reliable based on specific criteria. It uses the `flag` column to filter out unreliable estimations and then further filters based on a signal-to-noise ratio criterion defined in the `KalmanFilter` object.
#'
#' @param x A `KalmanFilter` object that includes attributes `@min_signal_to_noise` and `@max_signal_to_noise`, which define the acceptable range for the signal-to-noise ratio of the R estimations.
#' @param dataset_with_R A data frame or similar object that includes the R estimations and additional columns for diagnostics, specifically a `flag` column indicating the success of each estimation, and a `signal_to_noise` column providing the signal-to-noise ratio for each estimation.
#' @return A filtered version of the `dataset_with_R` where only rows with a `flag` of 0 (indicating successful estimation) and a signal-to-noise ratio within the specified bounds are retained. This results in a subset of the original dataset that meets the reliability and quality criteria set forth by the `KalmanFilter` object.
#' @export
#' @examples
#' # Assuming `kf` is a properly initialized `KalmanFilter` object with `min_signal_to_noise` and `max_signal_to_noise` attributes set,
#' # and `dataset_with_R` is a dataframe containing R estimations and the required diagnostic columns:
#' filtered_dataset <- filter_R_estimations(kf, dataset_with_R)
#' head(filtered_dataset)
#' @method filter_R_estimations KalmanFilter
setGeneric("filter_R_estimations",
           function(x,dataset_with_R) standardGeneric("filter_R_estimations"))
setMethod("filter_R_estimations","KalmanFilter", function(x,dataset_with_R) {
  wrk_set <- dataset_with_R
  reliable_mask <- dataset_with_R$flag == 0
  wrk_set <- wrk_set[reliable_mask,]
  good_ratio <- (wrk_set$signal_to_noise <= x@min_signal_to_noise | wrk_set$signal_to_noise >= x@max_signal_to_noise)
  wrk_set <-  wrk_set[!good_ratio,]
  return(wrk_set)
})

#' Calculate Confidence Intervals for R Estimations
#'
#' This method calculates the confidence intervals for the effective reproduction number (R) estimations within a filtered dataset obtained from a `KalmanFilter` object. It adds confidence interval bounds for two specified alpha levels to the dataset.
#'
#' @param x A `KalmanFilter` object that contains alpha levels (`@alpha1`, `@alpha2`) for calculating the confidence intervals and `@gamma` used to derive the days infectious from the estimated R values.
#' @param filtered_dataset_with_R A data frame or similar object that has been filtered based on reliability and signal-to-noise ratio. It must include columns for R estimations (`R`) and their standard error (`se_R`).
#' @return The input dataset augmented with four new columns containing the upper and lower bounds of the confidence intervals for each of the alpha levels specified in the `KalmanFilter` object (`@alpha1`, `@alpha2`). Additionally, it includes a calculated column for days infectious derived from the `@gamma` parameter.
#' @export
#' @examples
#' # Assuming `kf` is a `KalmanFilter` object with set alpha levels and gamma,
#' # and `filtered_dataset_with_R` is a preprocessed dataset:
#' ci_dataset <- get_confident_intervals(kf, filtered_dataset_with_R)
#' head(ci_dataset)
#' @method get_confident_intervals KalmanFilter
setGeneric("get_confident_intervals",
           function(x,filtered_dataset_with_R) standardGeneric("get_confident_intervals"))
setMethod("get_confident_intervals","KalmanFilter", function(x,filtered_dataset_with_R) {
  colnames <- c(
    paste0("ci_",1-x@alpha1,"_u"),paste0("ci_",1-x@alpha1,"_l"),
    paste0("ci_",1-x@alpha2,"_u"),paste0("ci_",1-x@alpha2,"_l")
  )
  filtered_dataset_with_R[[colnames[1]]] <- NA
  filtered_dataset_with_R[[colnames[2]]] <- NA
  filtered_dataset_with_R[[colnames[3]]] <- NA
  filtered_dataset_with_R[[colnames[4]]] <- NA
  t_crit = c(
    qnorm(1 - x@alpha1 / 2),qnorm(1 - x@alpha2 / 2)
  )
  filtered_dataset_with_R[[colnames[1]]] <- filtered_dataset_with_R$R + t_crit[1]*filtered_dataset_with_R$se_R
  filtered_dataset_with_R[[colnames[2]]] <- filtered_dataset_with_R$R - t_crit[1]*filtered_dataset_with_R$se_R
  filtered_dataset_with_R[[colnames[3]]] <- filtered_dataset_with_R$R + t_crit[2]*filtered_dataset_with_R$se_R
  filtered_dataset_with_R[[colnames[4]]] <- filtered_dataset_with_R$R - t_crit[2]*filtered_dataset_with_R$se_R
  
  filtered_dataset_with_R$days_infectious <- NA
  filtered_dataset_with_R$days_infectious <- 1/x@gamma
  return(filtered_dataset_with_R)
})

#' Summarize Optimization Results by Country/Region
#'
#' This method aggregates optimization result metrics from a filtered dataset obtained from a `KalmanFilter` object, grouping by `Country/Region`. For each group, it retains the first instance of each metric within the group. The summarized optimization results are then stored back in the `KalmanFilter` object.
#'
#' @param x A `KalmanFilter` object that will store the aggregated optimization results in its `@optim` attribute.
#' @param filtered_dataset_with_R A data frame or similar object that contains the filtered estimations and diagnostics for R calculations. It must include a `Country/Region` column for grouping, along with columns for optimization flags (`flag`), variances (`sigma2_irregular`, `sigma2_level`), and signal-to-noise ratio (`signal_to_noise`).
#' @return The `KalmanFilter` object `x`, updated to include the aggregated optimization results in its `@optim` attribute. The aggregation process groups the data by `Country/Region` and summarizes each group with the first occurrence of each diagnostic metric.
#' @export
#' @examples
#' # Assuming `kf` is a `KalmanFilter` object,
#' # and `filtered_dataset_with_R` is a dataset with the necessary structure and diagnostics:
#' kf_with_optims <- get_Optims(kf, filtered_dataset_with_R)
#' # The `kf_with_optims@optim` now contains the summarized optimization results.
#' @method get_Optims KalmanFilter
setGeneric("get_Optims",
           function(x,filtered_dataset_with_R) standardGeneric("get_Optims"))
setMethod("get_Optims","KalmanFilter", function(x,filtered_dataset_with_R) {
  
  df_optim_res <- filtered_dataset_with_R %>%
    group_by(`Country/Region`) %>%
    summarise(flag = first(flag),
              sigma2_irregular = first(sigma2_irregular),
              sigma2_level = first(sigma2_level),
              signal_to_noise = first(signal_to_noise)) %>%
    ungroup()
  x@optim <- df_optim_res
  return(x)
})


#setGeneric("KalmanFiltering", function(x) standardGeneric("KalmanFiltering"))
#setMethod("KalmanFiltering","Flow", function(x) {
#  KF <- new("KalmanFilter",
#            dataset = x@dataset,
#            output_folder = "./data/estimate",
#            output_filename= "Kalman_filter.csv",
#            min_T = 20,
#            gamma = 1/7,
#            min_signal_to_noise = 0.01,
#            max_signal_to_noise = 0.25,
#            days_infection = 7,
#            n_start_values_grid = 0,
#            maxiter = 200,
#            optim = data.frame(),
#            alpha1=0.05,
#            alpha2 = 0.35
#  )
#  
#  KF <- impose_minimum_timeseries_observations(KF)
#  dataset_with_R <- make_R_estimations(KF)
#  filtered_dataset_with_R <- filter_R_estimations(KF,dataset_with_R)
#  KF <- get_Optims(KF,filtered_dataset_with_R)
#  KF@dataset <- get_confident_intervals(KF,filtered_dataset_with_R)
#  x@KF <- KF
#  return(x)
#})
#
#wrk_flow <- KalmanFiltering(wrk_flow)