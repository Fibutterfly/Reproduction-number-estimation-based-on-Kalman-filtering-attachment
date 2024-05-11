rm(list = ls())


require(methods)
library(dplyr)
library(tidyr)
source("R/KalmanFilter.R")
source("R/cori_estim.R")

setClass("Flow",
  slots = c(
    list_of_urls = "list",
    dataset = "data.frame",
    days_infection_vector = "numeric",
    min_cases = "numeric",
    end_date = "character",
    restrict_end_sample = "logical",
    KF = "KalmanFilter",
    Bayesian = "data.frame",
    epiestim = "EpiEstimWrapper"
  ),
  prototype = c(
    list_of_urls = list(),
    dataset = data.frame(),
    min_cases = 100,
    end_date = "2020-05-06",
    restrict_end_sample = FALSE
  )
         
)

source("R/datagen.R")
source("R/helpers.R")
setGeneric("getData", function(x) standardGeneric("getData"))
setMethod("getData","Flow", function(x) {
  url_vector <- unlist(x@list_of_urls)
  path <- paste0(getwd(),"/data/original")
  print(path)
  cleanFolder(path)
  lapply(names(url_vector), function(name) {
    dataGenObj <- new("DataGen", url = url_vector[[name]], export_folder = path, file_name=name)
    loadAndWriteData(dataGenObj)
    return(dataGenObj)
  })
})



url_list <- list(
  time_series_covid19_confirmed_global.csv = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv',
  time_series_covid19_recovered_global.csv = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv',
  time_series_covid19_deaths_global.csv    = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv',
  interventions.csv       = 'https://raw.githubusercontent.com/ImperialCollegeLondon/covid19model/master/data/interventions.csv',
  Global_Mobility_Report.csv  = 'https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv',
  R_EpiEstim.csv  = 'https://hsph-covid-study.s3.us-east-2.amazonaws.com/website_files/rt_table_export.csv.zip')

wrk_flow <- new("Flow", list_of_urls = url_list, 
                days_infection_vector = c(5, 6, 7, 8, 9, 10))
getData(wrk_flow)

source("R/contruct_dataset.R")
setGeneric("constructData", function(x) standardGeneric("constructData"))
setMethod("constructData","Flow", function(x) {
  in_path <- paste0(getwd(),"/data/original")
  out_path <- paste0(getwd(),"/data/processed")
  
  cleanFolder(out_path)
  CD <- new("ConstructDataset")
  TC <- loadAndCleanCSV(CD,"time_series_covid19_confirmed_global.csv",
                        "total_cases")
  RG <- loadAndCleanCSV(CD,"time_series_covid19_recovered_global.csv",
                        "total_recovered")
  TC <- merge(TC, RG, by = c("Country/Region", "Date"), all.x = TRUE)
  RG <- NULL
  DG <- loadAndCleanCSV(CD,"time_series_covid19_deaths_global.csv",
                        "total_deaths")
  TC <- merge(TC, DG, by = c("Country/Region", "Date"), all.x = TRUE)
  DG <- NULL
  x@dataset <- TC
  return(x)
})

wrk_flow <- constructData(wrk_flow)

source("R/transformDataset.R")
wrk_flow <- transformDataset(wrk_flow)


source("R/dataset_tests.R")
tests_positive_infected(wrk_flow)
test_new_cases(wrk_flow)
tests_growth_rates(wrk_flow)
tests_growth_rates_2(wrk_flow)

setGeneric("KalmanFiltering", function(x) standardGeneric("KalmanFiltering"))
setMethod("KalmanFiltering","Flow", function(x) {
  KF <- new("KalmanFilter",
            dataset = x@dataset,
            output_folder = "./data/estimate",
            output_filename= "Kalman_filter.csv",
            min_T = 20,
            gamma = 1/7,
            min_signal_to_noise = 0.01,
            max_signal_to_noise = 0.25,
            days_infection = 7,
            n_start_values_grid = 0,
            maxiter = 200,
            optim = data.frame(),
            alpha1=0.05,
            alpha2 = 0.35
  )
  
  KF <- impose_minimum_timeseries_observations(KF)
  start_time <- Sys.time()
  dataset_with_R <- make_R_estimations(KF)
  filtered_dataset_with_R <- filter_R_estimations(KF,dataset_with_R)
  KF <- get_Optims(KF,filtered_dataset_with_R)
  KF@dataset <- get_confident_intervals(KF,filtered_dataset_with_R)
  end_time <- Sys.time()
  print(end_time-start_time)
  x@KF <- KF
  return(x)
})

wrk_flow <- KalmanFiltering(wrk_flow)


source("R/estimates_tests.R")

test_new_cases(wrk_flow)
tests_positive_infected(wrk_flow)

source("R/plots.R")
setGeneric("KalmanPlots", function(x) standardGeneric("KalmanPlots"))
setMethod("KalmanPlots","Flow", function(x) {
  world_data <- x@KF@dataset[x@KF@dataset$`Country/Region`=="World",]
  world_plot <- standardPlot(world_data,"Date","R","se_R",
               "A világ reprodukciós rátája Kálmán Filterrel",
               "Dátum",
               "R")
  print(world_plot)
  
  China_data <- x@KF@dataset[x@KF@dataset$`Country/Region`=="China",]
  China_plot <- standardPlot(China_data,"Date","R","se_R",
                             "Kína reprodukciós rátája Kálmán Filterrel",
                             "Dátum",
                             "R")
  print(China_plot)
  
  Croatia_data <- x@KF@dataset[x@KF@dataset$`Country/Region`=="Croatia",]
  Croatia_plot <- standardPlot(Croatia_data,"Date","R","se_R",
                             "Horvátország reprodukciós rátája Kálmán Filterrel",
                             "Dátum",
                             "R")
  print(Croatia_plot)
  US_data <- x@KF@dataset[x@KF@dataset$`Country/Region`=="US",]
  US_plot <- standardPlot(US_data,"Date","R","se_R",
                               "Egyesült Államok reprodukciós rátája Kálmán Filterrel",
                               "Dátum",
                               "R")
  print(US_plot)
})

KalmanPlots(wrk_flow)

setGeneric("loadBayesian", function(x,file_path) standardGeneric("loadBayesian"))
setMethod("loadBayesian","Flow", function(x, file_path) {
  temp <- read.csv(file_path, check.names = FALSE)
  temp <- temp %>%
    group_by(Date,`Country/Region`) %>%
    summarize(
      R = mean(R, na.rm = TRUE),
      ci_95_l = mean(ci_95_l, na.rm = TRUE),
      ci_95_u = mean(ci_95_u, na.rm = TRUE)
    )
  temp$Date <- as.Date(temp$Date, format = "%Y-%m-%d")
  x@Bayesian <- temp
  return(x)
})

wrk_flow <- loadBayesian(wrk_flow,"./data/static/estimated_R.csv")

source("R/plots.R")
setGeneric("BayesianPlots", function(x) standardGeneric("BayesianPlots"))
setMethod("BayesianPlots","Flow", function(x) {
  date_filtered <- x@Bayesian[,]
  world_data <- date_filtered[date_filtered$`Country/Region`=="World",]
  world_plot <- bayesianPlot(world_data,"Date","R","ci_95_l","ci_95_u",
                             "A világ reprodukciós rátája Bayesian Filterrel",
                             "Dátum",
                             "R")
  print(world_plot)
  
  China_data <- date_filtered[date_filtered$`Country/Region`=="China",]
  China_plot <- bayesianPlot(China_data,"Date","R","ci_95_l","ci_95_u",
                             "Kína reprodukciós rátája Bayesian Filterrel",
                             "Dátum",
                             "R")
  print(China_plot)
  
  Croatia_data <- date_filtered[date_filtered$`Country/Region`=="Croatia",]
  Croatia_plot <- bayesianPlot(Croatia_data,"Date","R","ci_95_l","ci_95_u",
                               "Horvátország reprodukciós rátája Bayesian Filterrel",
                               "Dátum",
                               "R")
  print(Croatia_plot)
  US_data <- date_filtered[date_filtered$`Country/Region`=="US",]
  US_plot <- bayesianPlot(US_data,"Date","R","ci_95_l","ci_95_u",
                          "Egyesült Államok reprodukciós rátája Bayesian Filterrel",
                          "Dátum",
                          "R")
  print(US_plot)
})

BayesianPlots(wrk_flow)

source("R/plots.R")
setGeneric("KalmanVsBayesPlots", function(x) standardGeneric("KalmanVsBayesPlots"))
setMethod("KalmanVsBayesPlots","Flow", function(x) {
  temp_world <- datasetGenForVsPlot(wrk_flow@KF@dataset, wrk_flow@Bayesian,
                                    "Country/Region","World", "Filter", "Kálmán"
                                    ,"Bayesian",c("Date","R"))
  temp_china <- datasetGenForVsPlot(wrk_flow@KF@dataset, wrk_flow@Bayesian,
                                    "Country/Region","China", "Filter", "Kálmán"
                                    ,"Bayesian",c("Date","R"))
  temp_croatia <- datasetGenForVsPlot(wrk_flow@KF@dataset, wrk_flow@Bayesian,
                                      "Country/Region","Croatia","Filter",
                                      "Kálmán","Bayesian",c("Date","R"))
  temp_us <- datasetGenForVsPlot(wrk_flow@KF@dataset, wrk_flow@Bayesian,
                                      "Country/Region","US","Filter",
                                      "Kálmán","Bayesian",c("Date","R"))
  
  world_plot <- genVsPlots(temp_world,x_key = "Date", y_key = "R"
                           , group_key = "Filter",
in_title = "A világ reprodukciós rátája Kálmán - és Bayesian - Filterrel",
in_x_label = "Dátum", in_y_label = "R")
  china_plot <- genVsPlots(temp_china,x_key = "Date", y_key = "R"
                           , group_key = "Filter",
in_title = "Kína reprodukciós rátája Kálmán - és Bayesian - Filterrel",
                           in_x_label = "Dátum", in_y_label = "R")

  croatia_plot <- genVsPlots(temp_croatia,x_key = "Date", y_key = "R"
                           , group_key = "Filter",
in_title = "Horvátor. reprodukciós rátája Kálmán - és Bayesian - Filterrel",
                           in_x_label = "Dátum", in_y_label = "R")  
  
  us_plot <- genVsPlots(temp_us,x_key = "Date", y_key = "R"
                             , group_key = "Filter",
 in_title = "USA reprodukciós rátája Kálmán - és Bayesian - Filterrel",
                             in_x_label = "Dátum", in_y_label = "R")  
  print(world_plot)
  print(china_plot)
  print(croatia_plot)
  print(us_plot)
})
KalmanVsBayesPlots(wrk_flow)


source("R/cori_estim.R")
setGeneric("makeEstim", function(x) standardGeneric("makeEstim"))
setMethod("makeEstim","Flow", function(x) {
  temp <- new("EpiEstimWrapper", o_ds = x@dataset)
  start_time <- Sys.time()
  temp <- getEstimTable(temp)
  end_time <- Sys.time()
  time_taken <- end_time - start_time
  print(time_taken)
  #temp <- getWallingaTable(temp)
  x@epiestim <- temp
  return(x)
})

wrk_flow <- makeEstim(wrk_flow)

setGeneric("estimPlots", function(x) standardGeneric("estimPlots"))
setMethod("estimPlots","Flow", function(x) {
  temp_world <- x@epiestim@cori_results$World
  temp_china <- x@epiestim@cori_results$China
  temp_croatia <- x@epiestim@cori_results$Croatia
  temp_us <- x@epiestim@cori_results$US
  world_plot <- standardPlot(temp_world,x_name = "Date",y_name = "mean_R",
                             y_err = "se_R",
              in_title = "A világ reprodukciós rátája EpiEstim algoritmussal.",
              in_x_label = "Dátum",
              in_y_label = "R")
  china_plot <- standardPlot(temp_china,x_name = "Date",y_name = "mean_R",
                             y_err = "se_R",
                in_title = "Kína reprodukciós rátája EpiEstim algoritmussal.",
                             in_x_label = "Dátum",
                             in_y_label = "R")
  croatia_plot <- standardPlot(temp_croatia,x_name = "Date",y_name = "mean_R",
                             y_err = "se_R",
         in_title = "Horvátország reprodukciós rátája EpiEstim algoritmussal.",
                             in_x_label = "Dátum",
                             in_y_label = "R")
  us_plot <- standardPlot(temp_us,x_name = "Date",y_name = "mean_R",
                             y_err = "se_R",
     in_title = "Egyesült Államok reprodukciós rátája EpiEstim algoritmussal.",
                             in_x_label = "Dátum",
                             in_y_label = "R")
  
  
  print(world_plot)
  print(china_plot)
  print(croatia_plot)
  print(us_plot)
})

estimPlots(wrk_flow)

setGeneric("estimVsKalman", function(x) standardGeneric("estimVsKalman"))
setMethod("estimVsKalman","Flow", function(x) {
  temp_world <- x@epiestim@cori_results$World
  temp_china <- x@epiestim@cori_results$China
  temp_croatia <- x@epiestim@cori_results$Croatia
  temp_us <- x@epiestim@cori_results$US
  temp_world[["R"]] <- temp_world[["mean_R"]]
  temp_world[["Country/Region"]] <- "World"
  temp_china[["R"]] <- temp_china[["mean_R"]]
  temp_china[["Country/Region"]] <- "China"
  temp_croatia[["R"]] <- temp_croatia[["mean_R"]]
  temp_croatia[["Country/Region"]] <- "Croatia"
  temp_us[["R"]] <- temp_us[["mean_R"]]
  temp_us[["Country/Region"]] <- "US"
  temp_complex <- rbind(temp_world,temp_china,temp_croatia, temp_us)
  
  temp_world <- datasetGenForVsPlot(wrk_flow@KF@dataset, temp_complex,
                                    "Country/Region","World", "Filter", "Kálmán"
                                    ,"Cori",c("Date","R"))
  temp_china <- datasetGenForVsPlot(wrk_flow@KF@dataset, temp_complex,
                                    "Country/Region","China", "Filter", "Kálmán"
                                    ,"Cori",c("Date","R"))
  temp_croatia <- datasetGenForVsPlot(wrk_flow@KF@dataset, temp_complex,
                                      "Country/Region","Croatia","Filter",
                                      "Kálmán","Cori",c("Date","R"))
  temp_us <- datasetGenForVsPlot(wrk_flow@KF@dataset, temp_complex,
                                 "Country/Region","US","Filter",
                                 "Kálmán","Cori",c("Date","R"))
  world_plot <- genVsPlots(temp_world,x_key = "Date", y_key = "R"
                           , group_key = "Filter",
      in_title = "A világ reprodukciós rátája Kálmán Filterrel és Cori módszerrel",
    in_x_label = "Dátum", in_y_label = "R")
  china_plot <- genVsPlots(temp_china,x_key = "Date", y_key = "R"
                         , group_key = "Filter",
       in_title = "Kína reprodukciós rátája Kálmán Filterrel és Cori módszerrel",
                         in_x_label = "Dátum", in_y_label = "R")

  croatia_plot <- genVsPlots(temp_croatia,x_key = "Date", y_key = "R"
                           , group_key = "Filter",
   in_title = "Horvátor. reprodukciós rátája Kálmán Filterrel és Cori módszerrel",
                           in_x_label = "Dátum", in_y_label = "R")  

  us_plot <- genVsPlots(temp_us,x_key = "Date", y_key = "R"
                      , group_key = "Filter",
      in_title = "USA reprodukciós rátája Kálmán Filterrel és Cori módszerrel",
                      in_x_label = "Dátum", in_y_label = "R")  
  print(world_plot)
  print(china_plot)
  print(croatia_plot)
  print(us_plot)
})

estimVsKalman(wrk_flow)


setGeneric("wallingaPlots", function(x) standardGeneric("wallingaPlots"))
setMethod("wallingaPlots","Flow", function(x) {
  temp_world <- x@epiestim@wallinga_results$World
  temp_china <- x@epiestim@wallinga_results$China
  temp_croatia <- x@epiestim@wallinga_results$Croatia
  temp_us <- x@epiestim@wallinga_results$US
  world_plot <- standardPlot(temp_world,x_name = "Date",y_name = "mean_R",
                             y_err = "se_R",
                             in_title = "A világ reprodukciós rátája Wallinga-Tannuis módszerrel.",
                             in_x_label = "Dátum",
                             in_y_label = "R")
  china_plot <- standardPlot(temp_china,x_name = "Date",y_name = "mean_R",
                             y_err = "se_R",
                             in_title = "Kína reprodukciós rátája Wallinga-Tannuis módszerrel.",
                             in_x_label = "Dátum",
                             in_y_label = "R")
  croatia_plot <- standardPlot(temp_croatia,x_name = "Date",y_name = "mean_R",
                               y_err = "se_R",
                               in_title = "Horvátország reprodukciós rátája Wallinga-Tannuis módszerrel.",
                               in_x_label = "Dátum",
                               in_y_label = "R")
  us_plot <- standardPlot(temp_us,x_name = "Date",y_name = "mean_R",
                          y_err = "se_R",
                          in_title = "Egyesült Államok reprodukciós rátája Wallinga-Tannuis módszerrel.",
                          in_x_label = "Dátum",
                          in_y_label = "R")
  
  
  print(world_plot)
  print(china_plot)
  print(croatia_plot)
  print(us_plot)
})

#wallingaPlots(wrk_flow)