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
#'@import ggplot2
plotExData <- function(exdata,
                       bindwidth=NULL,
                       xrange=NULL,
                       ymax=NULL,
                       showLegend=T){
  gPlot <-
    ggplot2::ggplot(exdata, ggplot2::aes(x=val, fill = site_id)) +
    ggplot2::geom_histogram(binwidth = bindwidth) +
    ggplot2::scale_fill_discrete("Site") +
    ggplot2::labs(x=expression(theta), y='Counts') +
    ggplot2::theme(axis.text.y = ggplot2::element_text(colour="#00A4E6", size=14),
                   axis.text.x = ggplot2::element_text(size=13),
                   text = ggplot2::element_text(),
                   legend.background = ggplot2::element_rect(colour = "black"))

  if(!(is.null(xrange))){
    gPlot <- gPlot +  ggplot2::scale_x_continuous(limits = xrange)
  }

  if(!(is.null(ymax))){
    gPlot <- gPlot +  ggplot2::ylim(0,ymax)
  }

  if(!showLegend){gPlot + ggplot2::theme(legend.position="none")}

  gPlot

}
