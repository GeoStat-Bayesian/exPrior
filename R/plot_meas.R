#'plot histogram of measurements
#'
#'\code{plot_meas} plots histogram measurements as provided to generalFromMeas
#'
#'@param meas a dataframe containing measurements, as provided to the function generalFromMeas
#'@param bindwidth a numeric specifying the width of the bins (optional)
#'@return a plot
#'@export
plot_meas <- function(meas,bindwidth=NULL,showLegend=T){
  gPlot <- 
    ggplot(meas, aes(x=val, fill = site_id))+
    geom_histogram(binwidth = bindwidth)+
    scale_fill_discrete("Site") +
    labs(x = expression(theta), y='Counts') +
    theme( axis.text.y = element_text(colour="#00A4E6", size=14),
           axis.text.x = element_text(size = 13),
           text = element_text(),
           legend.background = element_rect(colour = "black"))
  if(showLegend){
    gPlot
  }else{
    gPlot + theme(legend.position="none")
  }
    
}


