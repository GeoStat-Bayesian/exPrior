#'plot prior and posterior distribution of hyperparameters
#'
#'\code{plot_hyperDist} plot prior and posterior distribution of hyperparameters
#'
#'@param res_gPrior output from the generalFromMeas function
#'@return a plot
#'@export
plot_hyperDist <- function(res_gPrior){

  # extract lists of priors and posteriors from res_gprior
  d_prior <- res_gPrior$d_hyperPar$d_hyperPar_prior
  d_post <- res_gPrior$d_hyperPar$d_hyperPar_post

  # extract list of hyperparameters names
  hyperPar <- res_gPrior$hyperPar

  # setup plotting settings on 2 rows and  2 columns
  par(mfrow = c(2, 2), mar=c(0,0,0,0), mai = c(0.8, 0.6, .1, 0.1))

  # loop over hyperparameters
  for(i in 1:length(d_prior)){
    plot(NA,
         xlim = range(c(d_prior[[i]]$x,d_post[[i]]$x)),
         ylim=range(c(0,d_prior[[i]]$y,d_post[[i]]$y)),
         xlab = '',
         ylab = '')
    mtext(text = bquote(.(as.name(hyperPar[i]))), side=1,line=2)
    mtext(text = bquote('p(' * .(as.name(hyperPar[i])) * '|'*theta*'*)'),side = 2,line = 2)
    lines(x = d_post[[i]]$x,d_post[[i]]$y,col='blue')
    lines(x = d_prior[[i]]$x,d_prior[[i]]$y,col='black')
    legend('topright',legend = c(bquote('p(' * .(as.name(hyperPar[i]))*')'),
                                 bquote('p(' * .(as.name(hyperPar[i])) * '|'*theta*'*)')),
           lty=1,col=c('blue','black'),bg = "white")
  }

}
