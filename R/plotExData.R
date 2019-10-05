#'plot histogram of measurements
#'
#'\code{plotExData} plots histogram of ex-situ data as provided to genExPrior
#'
#'@param exdata a dataframe containing ex-situ data, as provided to the function genExPrior
#'@param bindwidth a numeric specifying the width of the bins (optional)
#'@param showLegend a boolean indicating whether to show the legend
#'with the names of sites (optional, defaults to true)
#'@param xrange a vector with the limits for the x-axis site
#'@param ymax is a numeric specifying the maximum value on the y-axis
#'@return a plot
#'@export
plotExData <- function(exdata,
                       bindwidth=NULL,
                       xrange=NULL,
                       ymax=NULL,
                       showLegend=T){
  gPlot <-
    ggplot(exdata, aes(x=val, fill = site_id)) +
    geom_histogram(binwidth = bindwidth) +
    scale_fill_discrete("Site") +
    labs(x=expression(theta), y='Counts') +
    theme(axis.text.y = element_text(colour="#00A4E6", size=14),
          axis.text.x = element_text(size=13),
          text = element_text(),
          legend.background = element_rect(colour = "black"))

  if(!(is.null(xrange))){
    gPlot <- gPlot +  scale_x_continuous(limits = xrange)
  }

  if(!(is.null(ymax))){
    gPlot <- gPlot +  ylim(0,ymax)
  }

  if(!showLegend){gPlot + theme(legend.position="none")}

  gPlot

}
