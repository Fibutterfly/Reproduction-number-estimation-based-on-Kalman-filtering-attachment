setGeneric("tests_growth_rates_2", function(x) standardGeneric("tests_growth_rates_2"))
setMethod("tests_growth_rates_2","Flow", function(x) {
  mask <- logical(nrow(x@dataset))
  mask_newcases <- logical(nrow(x@dataset))
  mask_newcases <- x@dataset[['new_cases']] == 0 | mask_newcases
  mask_newcases[is.na(mask_newcases)] <- FALSE
  for (days_infectious in 5:10) {
    gr_infected_col_name <- paste0("gr_infected_", days_infectious)
    gr_mask <- !is.na(x@dataset[[gr_infected_col_name]])
    mask <- mask_newcases & gr_mask | mask
    target_value <- (-1 / days_infectious)*1.05
    comparison_result <- mask & x@dataset[[gr_infected_col_name]] <= target_value
    if ( any(comparison_result)) {
      print(x@dataset[comparison_result,])
      stop("Assertion failed: There are rows with negative values for new cases.")
    } else {
      cat("test passed: No negative values for new cases.\n")
    }
  }
})

setGeneric("tests_growth_rates", function(x) standardGeneric("tests_growth_rates"))
setMethod("tests_growth_rates","Flow", function(x) {
  mask <- logical(nrow(x@dataset))
  mask_newcases <- x@dataset[['new_cases']] == 0
  mask_newcases[is.na(mask_newcases)] <- FALSE
  for (days_infectious in 5:10) {
    gr_infected_col_name <- paste0("gr_infected_", days_infectious)
    gr_mask <- !is.na(x@dataset[[gr_infected_col_name]])
    mask <- mask_newcases & gr_mask
    target_value <- -1 / days_infectious
    comparison_result <- all.equal(x@dataset[[gr_infected_col_name]][mask], 
                                   rep(target_value, 
                                       length(x@dataset[[gr_infected_col_name]][mask]))
                                   , tolerance = 1e-8)
    if (!isTRUE(comparison_result)) {
      error_message <- paste("Assertion failed for column", gr_infected_col_name, 
                             ": Expected all values to be close to", target_value, 
                             ". Differences: ", comparison_result)
      stop(error_message)
    }
  }
})

setGeneric("test_new_cases", function(x) standardGeneric("test_new_cases"))
setMethod("test_new_cases","Flow", function(x) {
  mask <- logical(nrow(x@dataset))
  mask <- x@dataset[['new_cases']] < 0
  mask[is.na(mask)] <- FALSE
  if (nrow(x@dataset[mask,]) != 0) {
    stop("Assertion failed: There are rows with negative values for new cases.")
  } else {
    cat("test passed: No negative values for new cases.\n")
  }
  
})

setGeneric("tests_positive_infected", function(x) standardGeneric("tests_positive_infected"))
setMethod("tests_positive_infected","Flow", function(x) {
  mask <- logical(nrow(x@dataset))
  for (days_infectious in 5:10) {
    # Construct column name
    column_name <- paste0("infected_", days_infectious)
    
    # Update mask to include rows where the infected count is negative for this days_infectious
    temp_mask <- x@dataset[[column_name]] < 0
    temp_mask[is.na(temp_mask)] <- FALSE
    mask <- mask | temp_mask 
  }
  
  if (nrow(x@dataset[mask,]) != 0) {
    stop("Assertion failed: There are rows with negative values for infected individuals.")
  } else {
    cat("tests passed: No negative values for infected individuals.\n")
  }
  print(x@dataset[mask,])
})