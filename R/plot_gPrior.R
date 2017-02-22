#'plot general prior
#'
#'\code{plot_gPrior} plot informative and non-informative priors
#'
#'@param res_gPrior output from the generalFromMeas function
#'@param plotMeas boolean asking whether to plot the measurements
#'@return a plot
#'@export
plot_gPrior <- function(res_gPrior,plotMeas=F){

  if(plotMeas) {par(mar=0.1+c(4,4,1,4))}
  plot(res_gPrior$gPrior$x,res_gPrior$gPrior$y,
       type='l',col='red',
       xlab=expression(theta),ylab=expression(p(theta)))
  lines(x=res_gPrior$uPrior$x,y=res_gPrior$uPrior$y)
  legend('topright',c(expression(p(theta)),expression(p(theta*'|'*theta* '*'))),
         lty=1,col=c('black','red'))


  if(plotMeas){

    colVect <- rainbow(length(res_gPrior$meas))

    histList=list()
    for(i in 1:length(res_gPrior$meas)){
      histList[[i]] <- hist(res_gPrior$meas[[i]],plot = F)
    }

    # get maximum number of count
    maxCount=0
    for(i in 1:length(res_gPrior$meas)){
      if(max(histList[[i]]$counts)>maxCount){maxCount<-max(histList[[i]]$counts)}
    }

    par(new=T)
    plot(NA,NA,xaxt='n',yaxt='n',xlab='',ylab='',
         xlim=range(res_gPrior$gPrior$x),ylim=c(0,maxCount))
    axis(side = 4)
    mtext('counts per site', side=4, line=3)
    for(i in 1:length(res_gPrior$meas)){
      for(j in 1:length(histList[[i]]$counts)){
        polygon(x = c(histList[[i]]$mids[j],
                      histList[[i]]$mids[j],
                      histList[[i]]$mids[j],
                      histList[[i]]$mids[j]),
                y = c(0,0,
                      histList[[i]]$counts[j],
                      histList[[i]]$counts[j]),
                col=NA,
                border=adjustcolor( colVect[i], alpha.f = 0.5))
      }

    }

  }

}
