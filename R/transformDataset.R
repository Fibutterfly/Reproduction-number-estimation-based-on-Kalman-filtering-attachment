require(methods)

setGeneric("transformDataset", function(x) standardGeneric("transformDataset"))
setMethod("transformDataset","Flow", function(x) {
  df <- subset(x@dataset, total_cases >= x@min_cases)
  df_temp <- df %>%
    group_by(Date) %>%
    summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE))) %>%
    ungroup() %>%
    mutate(`Country/Region` = 'World')
  
  # Concatenate df_temp with the original df
  df <- rbind(df_temp, df)
  df_temp <- NULL
  
  df$Date <- as.Date(df$Date, format = "%m/%d/%y")
  
  df <- subset(df, `Country/Region` != 'Diamond Princess')
  
  df <- df %>%
    mutate(`Country/Region` = case_when(
      `Country/Region` == "Taiwan*" ~ "Taiwan",
      `Country/Region` == "Korea, South" ~ "South Korea",
      TRUE ~ `Country/Region`
    ))
  df <- df %>%
    arrange(`Country/Region`, Date)
  
  var_names <- c("cases", "recovered", "deaths")
  
  for (var_name in var_names) {
    total_var_name <- paste0("total_", var_name)
    new_var_name <- paste0("new_", var_name)
    
    df <- df %>%
      group_by(`Country/Region`) %>%
      mutate(!!new_var_name := !!sym(total_var_name) - lag(!!sym(total_var_name), default = 0)) %>%
      ungroup()
  }
  for(days_infectious in x@days_infection_vector){
    column_name <- paste0("infected_", days_infectious)
    df <- mutate(df, !!column_name := NA_real_) # Use NA_real_ for numeric columns
    for (country in unique(df$`Country/Region`)){
      mask <- df$`Country/Region` == country
      filtered_df <- df[mask,]
      T <- nrow(filtered_df)
      infected <- rep(NA_real_, T)
      infected[1] <- filtered_df$total_cases[1]
      
      for(tt in 2:T) {
        gamma = 1 / days_infectious
        infected[tt] <- (1 - gamma) * infected[tt - 1] + pmax(filtered_df$new_cases[tt], 0)
        
      }
      column_name <- paste0("infected_", days_infectious)
      df[[column_name]][mask] <- infected
    }
  }
  # Creating a mask for rows where new_cases < 0
  mask <- df$new_cases < 0
  
  # Setting new_cases to NA where the mask is TRUE
  df$new_cases[mask] <- NA
  for(days_infectious in x@days_infection_vector){
    column_name <- paste0("infected_", days_infectious)
    df[[column_name]][mask] <- NA
  }
  for(days_infectious in x@days_infection_vector){
    gr_infected_col_name <- paste0("gr_infected_", days_infectious)
    infected_col_name <- paste0("infected_", days_infectious)
    
    df <- df %>%
      group_by(`Country/Region`) %>%
      mutate(!!sym(gr_infected_col_name) := ((.data[[infected_col_name]] / lag(.data[[infected_col_name]])) - 1)) %>%
      ungroup()
    mask <- df %>%
      group_by(`Country/Region`) %>%
      mutate(prev_infected_is_zero = lag(.data[[infected_col_name]], default = NA) == 0.0) %>%
      ungroup() %>%
      pull(prev_infected_is_zero)
    df[[infected_col_name]][mask] <- NA
  }
  # Deal with potential consecutive zeros in the number of infected
  for(days_infectious in x@days_infection_vector){
    gr_infected_col_name <- paste0("gr_infected_", days_infectious)
    infected_col_name <- paste0("infected_", days_infectious)
    
    df$prev_infected <- ave(df[[infected_col_name]], df$`Country/Region`, FUN = function(x) c(NA, head(x, -1)))
    mask <- with(df, (df[[infected_col_name]] == 0.0) & (prev_infected == 0.0))
    df$prev_infected <- NULL
    df[[gr_infected_col_name]][mask] <- - (1 / days_infectious)
  }
  
  # Set to NaN observations with very small
  # number of cases but very high growth rates
  # to avoid these observations acting as
  # large outliers
  for(days_infectious in x@days_infection_vector){
    gr_infected_col_name <- paste0("gr_infected_", days_infectious)
    infected_col_name <- paste0("infected_", days_infectious)
    gamma <- 1 / days_infectious
    mask <- (df[['new_cases']] <= 25) & (df[[gr_infected_col_name]] >= gamma*(5-1))
    df[[gr_infected_col_name]][mask] <- NA
    df[[infected_col_name]][mask] <- NA
  }
  
  # Set to NaN observations implausibly
  # high growth rates that are likely
  # due to data issues 
  for(days_infectious in x@days_infection_vector){
    gr_infected_col_name <- paste0("gr_infected_", days_infectious)
    infected_col_name <- paste0("infected_", days_infectious)
    gamma <- 1 / days_infectious
    mask <- df[[gr_infected_col_name]] >= gamma*(10-1)
    
    df[[gr_infected_col_name]][mask] <- NA
    df[[infected_col_name]][mask] <- NA
  }
  # Remove initial NaN values for growth rates
  for (country in unique(df$`Country/Region`)){
    mask <- df$`Country/Region` == country
    filtered_df <- df[mask,]
    T <- nrow(filtered_df)
    df[['days_since_min_cases']][mask] <- 0:(T-1)
  }
  mask <- df[['days_since_min_cases']] >= 1
  df <- df[mask,]
  df[['days_since_min_cases']] <- NULL
  
  if(x@restrict_end_sample)
  {
    mask <- df[Date] <- as.Date(x@restrict_end_sample, format = "%Y-%m-%d")
    df <- df[mask,]
  }
  x@dataset <- df
  return(x)
})