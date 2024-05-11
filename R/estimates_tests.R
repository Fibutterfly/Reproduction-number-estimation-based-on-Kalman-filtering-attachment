setGeneric("tests_positive_infected", function(x) standardGeneric("tests_positive_infected"))
setMethod("tests_positive_infected","Flow", function(x) {
  mask <- logical(nrow(x@dataset))
  infected_mask <- x@dataset[["infected_5"]] < 0.0
  for (days_infectious in 6:10) {
    colname <- paste0("infected_", days_infectious)
    infected_mask <- x@dataset[[colname]] < 0.0
    mask <- infected_mask | mask
  }
  mask[is.na(mask)] <- FALSE
  if (nrow(x@dataset[mask,]) == 0) {
    cat("test passed: No negative values for infected.\n")
  } else {
    View(mask)
    View(x@dataset[mask,])
    stop("Assertion failed: There are rows with negative values for infedted.")
  }
})



setGeneric("test_new_cases", function(x) standardGeneric("test_new_cases"))
setMethod("test_new_cases","Flow", function(x) {
  mask <- logical(nrow(x@dataset))
  new_cases_mask <- x@dataset[["new_cases"]] < 0.0
  new_cases_mask[is.na(new_cases_mask)] <- FALSE
  if (nrow(x@dataset[new_cases_mask,]) == 0) {
    cat("test passed: No negative values for new cases\n")
  } else {
    
    print(x@dataset[comparison_result,])
    stop("Assertion failed: There are rows with negative values for new cases.")
  }
})

