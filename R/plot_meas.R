#'plot histogram of measurements
#'
#'\code{plot_meas} plots histogram measurements as provided to generalFromMeas
#'
#'@param meas a dataframe containing measurements, as provided to the function generalFromMeas
#'@param bindwidth a numeric specifying the width of the bins (optional)
#'@param showLegend a boolean indicating whether to show the legend
#'with the names of sites (optional, defaults to true)
#'@param xrange a vector with the limits for the x-axis site
#'@return a plot
#'@export
plot_meas <- function(meas,
                      bindwidth=NULL,
                      xrange=NULL,
                      showLegend=T){
  gPlot <-
    ggplot(meas, aes(x=val, fill = site_id))+
    geom_histogram(binwidth = bindwidth)+
    scale_fill_discrete("Site") +
    labs(x = 'Y', y='Counts') +
    theme( axis.text.y = element_text(colour="#00A4E6", size=14),
           axis.text.x = element_text(size = 13),
           text = element_text(),
           legend.background = element_rect(colour = "black"))

  if(!(is.null(xrange))){
    gPlot <- gPlot +  scale_x_continuous(limits = xrange)
  }

  if(!showLegend){gPlot + theme(legend.position="none")}

  gPlot

}


