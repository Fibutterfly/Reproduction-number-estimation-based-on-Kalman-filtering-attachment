library(ggplot2)
library(patchwork)

#plot_test <- wrk_flow@KF@dataset[wrk_flow@KF@dataset$`Country/Region` == "China", ]
#
#plot_p <- ggplot()+ 
#          geom_line(data = plot_test, aes(x = Date, y = R))+
#          geom_point(data = plot_test, aes(x = Date, y = R))+
#          geom_errorbar(data = plot_test,
#            aes(x=Date,ymin = R - se_R, ymax = R+se_R), color="red",
#                alpha=0.2)+
#          labs(
#            title = "Kína reprodukciós rátája a járvány alatt Kálmán Filterrel",
#            x = "Dátum",
#            y = "Reprodukciós Ráta"
#          )

standardPlot <- function(ds, x_name, y_name, y_err, in_title="", in_x_label = "",
                         in_y_label="",err_alpha=0.15){
  plot_rtn <- ggplot()+ 
    geom_line(data = ds, aes(x = .data[[x_name]], y = .data[[y_name]]))+
    geom_point(data = ds, aes(x = .data[[x_name]], y = .data[[y_name]]),size=0.1)+
    geom_errorbar(data = ds,
                  aes(x=Date,ymin = .data[[y_name]] - .data[[y_err]], 
                      ymax = .data[[y_name]]+.data[[y_err]]), color="red",
                  alpha=err_alpha)+
    labs(
      title = in_title,
      x = in_x_label,
      y = in_y_label
    )
  return(plot_rtn)
}

bayesianPlot <- function(ds, x_name, y_name, lower_err, upper_err, in_title="", in_x_label = "",
                         in_y_label=""){
  plot_rtn <- ggplot()+ 
    geom_line(data = ds, aes(x = .data[[x_name]], y = .data[[y_name]]))+
    geom_errorbar(data = ds,
                  aes(x=Date,ymin = .data[[lower_err]], 
                      ymax = .data[[upper_err]]), color="red",
                  alpha=0.15)+
    labs(
      title = in_title,
      x = in_x_label,
      y = in_y_label
    )
  return(plot_rtn)
}
#print(plot_p)
#print(standardPlot(plot_test,"Date","R","se_R",
#                   "Kína reprodukciós rátája a járvány alatt Kálmán Filterrel",
#                   "Dátum","Reprodukciós Ráta"))

datasetGenForVsPlot <- function(ds1, ds2, country_key, country_value, labelname,
                                label1_value,label2_value, interesting_cols){
  temp1 <- ds1[ds1[[country_key]]==country_value,interesting_cols]
  temp1[[labelname]] <- rep(label1_value)
  temp2 <- ds2[ds2[[country_key]]==country_value,interesting_cols]
  temp2[[labelname]] <- rep(label2_value)
  return(rbind(temp1,temp2))
}

genVsPlots <- function(ds, x_key,y_key, group_key,in_title, in_x_label,
                       in_y_label, coloring=c('blue','red'),
                       legend_position="bottom"){
  mixed_plot <- ggplot(ds, aes(x = .data[[x_key]], y = .data[[y_key]],
                               group=.data[[group_key]]))+
    geom_line(aes(color=.data[[group_key]]))+
    scale_color_manual(values=coloring) +
    theme(legend.position=legend_position) +
    labs(
      title = in_title,
      x = in_x_label,
      y = in_y_label
    )
  
  return(mixed_plot)
}


# world_test <- datasetGenForVsPlot(wrk_flow@KF@dataset,
#                                   wrk_flow@Bayesian,"Country/Region",
#                     "World", "Filter", "Kálmán","Bayesian",c("Date","R"))
# genVsPlots(world_test,"Date","R","Filter", "Teszt","Dátum","R")

