#'plot general prior
#'
#'\code{plot_gPrior} plot informative and non-informative priors
#'
#'@param res_gPrior output from the generalFromMeas function
#'@param plotMeas boolean asking whether to plot the measurements
#'@return a plot
#'@export
plot_gPrior <- function(res_gPrior,plotMeas=F){

  # construct single dataframe with uninformative and informative priors
  # easier for ggplot2
  df_gPrior <- data.frame(theta=res_gPrior$uPrior$x,
                          uPrior=res_gPrior$uPrior$y,
                          gPrior=res_gPrior$gPrior$y)

  df_gPrior <- reshape2::melt(df_gPrior, id.vars = "theta")

  # define plot without histogram of values
  gPlot <- ggplot() +
    geom_line(data=df_gPrior,
              aes(x=theta,y=value,color=variable),
              size=1.5) +
    scale_colour_manual(values=c("#00A4E6","#68382C"),
                        labels=c(expression(f[Theta](theta)),
                                 expression(f[Theta](theta*'|'*theta^'*')))) +
    labs(x = expression(theta),y=expression(p(theta))) +
    theme(text = element_text(size=15),
          legend.title=element_blank(),
          legend.background = element_rect(colour = "black"))

  if(!plotMeas){

    gPlot # actually plots

  }else{ # here overlay priors with histrogram of measurements

    gPlot +
      geom_histogram(data=res_gPrior$meas, aes(val, fill=site_idx), alpha = .5) +
      labs(x = expression(theta),y=expression(p(theta))) +
      scale_fill_discrete("Site")
    # still need to put histogram on different scales...

  }

}
