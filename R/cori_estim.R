library(EpiEstim)
#data("Flu2009")
#
#temp_world <- wrk_flow@dataset[wrk_flow@dataset$`Country/Region`=="World",
#                                    c("Date","new_cases")]
#temp_world_cori_incidence <- data.frame(
#  dates = temp_world$Date,
#    I = temp_world$new_cases
#  
#)
#
#R_cori_world <- estimate_R(temp_world_cori_incidence,
#                           method = "parametric_si",
#                           config = make_config(list(
#                             mean_si = 5.19, 
#                             std_si = 4)))
#
#estimate_R_plots(R_cori_world, what = "R",
#options_R = list(col = c("blue")), legend = TRUE)

setClass("EpiEstimWrapper",
         slots = c(
           o_ds = "data.frame",
           mean_si = "numeric",
           std_si = "numeric",
           cori_method = "character",
           cori_results = "list",
           wallinga_method = "character",
           wallinga_results = "list"
         ),
         prototype = c(
           o_ds = data.frame(),
           mean_si = 5.19,
           std_si = 4,
           cori_method = "parametric_si",
           cori_results = list(),
           wallinga_method = "parametric_si",
           wallinga_results = list()
         )
         
)
setGeneric("calcEstim",
           function(x,incidence_ds) standardGeneric("calcEstim"))
setMethod("calcEstim","EpiEstimWrapper", function(x,incidence_ds) {
 return(estimate_R(incidence_ds,
                             method = x@cori_method,
                             config = make_config(list(
                               mean_si = x@mean_si, 
                               std_si = x@std_si))))
})

setGeneric("calcWallinga",
           function(x,incidence_ds) standardGeneric("calcWallinga"))
setMethod("calcWallinga","EpiEstimWrapper", function(x,incidence_ds) {
  in_t_start <- seq(2, nrow(incidence_ds)-6)
  in_t_end <- in_t_start + 6
  return(wallinga_teunis(incidence_ds,
                    method = x@wallinga_method,
                    config = make_config(list(
                      mean_si = x@mean_si, 
                      std_si = x@std_si,
                      t_start = in_t_start,
                      t_end = in_t_end))))
})

setGeneric("getEstimTable",
           function(x) standardGeneric("getEstimTable"))
setMethod("getEstimTable","EpiEstimWrapper", function(x) {
  result <- list()
  countries <- unique(x@o_ds$`Country/Region`)
  for (act_country in countries) {
    temp_df <- x@o_ds[x@o_ds$`Country/Region`==act_country,
                                c("Date","new_cases")]
    temp_incidence <- data.frame(
      dates = temp_df$Date,
      I = temp_df$new_cases
    )
    temp_incidence$I[is.na(temp_incidence$I)] <- 0
    temp_rtn <- calcEstim(x,temp_incidence)
    row_num <- length(temp_rtn$R$`Mean(R)`)
    rtn_frame <- data.frame(
      Date = tail(temp_rtn$dates,row_num),
      mean_R = temp_rtn$R$`Mean(R)`,
      se_R = temp_rtn$R$`Std(R)`,
      med_R = temp_rtn$R$`Median(R)`
    )
    #standardPlot(ds = rtn_frame,x_name = "Date",y_name = "mean_R",y_err = "se_R"
    #             , in_title = "teszt",in_x_label = "teszt",in_y_label = "teszt")
    result[[act_country]] <- rtn_frame
  }
  x@cori_results <- result
  return(x)
})

setGeneric("getWallingaTable",
           function(x,country_list) standardGeneric("getWallingaTable"))
setMethod("getWallingaTable","EpiEstimWrapper", function(x,country_list) {
  result <- list()
  #countries <- unique(x@o_ds$`Country/Region`)
  for (act_country in country_list) {
    temp_df <- x@o_ds[x@o_ds$`Country/Region`==act_country,
                      c("Date","new_cases")]
    temp_incidence <- data.frame(
      dates = temp_df$Date,
      I = temp_df$new_cases
    )
    temp_incidence$I[is.na(temp_incidence$I)] <- 0
    temp_rtn <- calcWallinga(x,temp_incidence)
    row_num <- length(temp_rtn$R$`Mean(R)`)
    rtn_frame <- data.frame(
      Date = tail(temp_rtn$dates,row_num),
      mean_R = temp_rtn$R$`Mean(R)`,
      se_R = temp_rtn$R$`Std(R)`
    )
    #standardPlot(ds = rtn_frame,x_name = "Date",y_name = "mean_R",y_err = "se_R"
    #             , in_title = "teszt",in_x_label = "teszt",in_y_label = "teszt")
    result[[act_country]] <- rtn_frame
  }
  x@wallinga_results <- result
  return(x)
})

#setGeneric("makeEstim", function(x) standardGeneric("makeEstim"))
#setMethod("makeEstim","Flow", function(x) {
#  temp <- new("EpiEstimWrapper", o_ds = x@dataset)
#  #temp <- getEstimTable(temp)
#  wallinga_country_list <- c("World","China","Croatia","US")
#  temp <- getWallingaTable(temp,wallinga_country_list)
#  x@epiestim <- temp
#  return(x)
#})
#
#wrk_flow <- makeEstim(wrk_flow)

# valszeg hátulról van előre felé feltöltve

#R_instantaneous <- estimate_R(Flu2009$incidence,
#                              method = "non_parametric_si",
#                              config = list(t_start = seq(2, 26), 
#                                            t_end = seq(8, 32), 
#                                            si_distr = Flu2009$si_distr
#                              )
#)
#
#R_case <- wallinga_teunis(Flu2009$incidence,
#                          method = "non_parametric_si",
#                          config = list(t_start = seq(2, 26), 
#                                        t_end = seq(8, 32), 
#                                        si_distr = Flu2009$si_distr
#                          )
#)
#estimate_R_plots(R_instantaneous, what = "R",
#                 options_R = list(col = c("blue")), legend = TRUE)
#
#R_weekly <- estimate_R(Flu2009$incidence,
#                       method = "non_parametric_si",
#                       config = list(t_start = seq(9, 26), 
#                                     t_end = seq(15, 32), 
#                                     si_distr = Flu2009$si_distr
#                       )
#)
#
#
#R_biweekly <- estimate_R(Flu2009$incidence,
#                         method = "non_parametric_si",
#                         config = list(t_start = seq(2, 19), 
#                                       t_end = seq(15, 32),  
#                                       si_distr = Flu2009$si_distr
#                         )
#)
#
#estimate_R_plots(list(R_weekly, R_biweekly), what = "R",
#                 options_R = list(col = c("blue", "red")), legend = TRUE)#