library(methods)
library(dplyr)
library(tidyr)
#' ConstructDataset: Az osztály célja, hogy adatokat letöltsön
#'
#' Egy bizonyos URL-ről letölti az adatokat és elnevezi őket tetszőleges módon,
#' Amennyiben zip fájlt talál az abban található CSV-ket is a megadott helyre
#' teszi.
#' @slot 
#' @export
setClass("ConstructDataset",
         slots = c(
           input_folder = "character",
           output_folder = "character"
         ),
         prototype = list(
           input_folder = "./data/original",
           output_folder = "./data/processed"
         )
         
)

setValidity("ConstructDataset",function(object) {
  if(!is.character(object@input_folder)) {
    return("@input_folder not character")
  }
  else if(!is.character(object@output_folder)){
    return("@output_folder not character")
  }
  else{
    return(TRUE)
  }
})

setGeneric("loadAndCleanCSV", function(x,file_name,var_name) 
  standardGeneric("loadAndCleanCSV"))
setMethod("loadAndCleanCSV","ConstructDataset", function(x,file_name,var_name) {
  # Read the CSV file
  file_path <- paste0(x@input_folder,"/",file_name)

  df <- read.csv(file_path, check.names = FALSE) # Avoid automatic renaming of columns like '1/1/21' to 'X1.1.21'
  
  # Remove 'Lat' and 'Long' columns
  df <- select(df, -Lat, -Long)
  
  # Melt to long format using pivot_longer for better readability and flexibility
  df <- pivot_longer(df, 
                     cols = -c(`Province/State`, `Country/Region`),
                     names_to = "Date", 
                     values_to = var_name)
  
  # Ensure var_name is used correctly in subsequent operations
  df <- df %>%
    group_by(`Country/Region`, Date) %>%
    summarize(!!sym(var_name) := sum(!!sym(var_name), na.rm = TRUE)) %>%
    ungroup()

  return(df)
})