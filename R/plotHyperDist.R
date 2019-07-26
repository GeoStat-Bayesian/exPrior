#'plot prior and posterior distribution of hyperparameters
#'
#'\code{plotHyperDist} plot prior and posterior distribution of hyperparameters
#'
#'@param resExPrior output from the genExPrior function
#'@return a plot
#'@export
plotHyperDist <- function(resExPrior){

  # extract lists of priors and posteriors from resExPrior
  d_prior <- resExPrior$d_hyperPar$d_hyperPar_prior
  d_post <- resExPrior$d_hyperPar$d_hyperPar_post

  # extract list of hyperparameters names
  hyperPar <- resExPrior$hyperPar
  g_list <- list()

  for(i in 1:length(hyperPar)){

    d_i <- data.frame(x=d_prior[[i]]$x,
                      prior=d_prior[[i]]$y,
                      post=d_post[[i]]$y)

    d_i_long <- reshape2::melt(data = d_i,id.vars ="x")

    g_list[[i]] <-
      ggplot(d_i_long, aes(x=x, y=value, color=variable)) +
      geom_line() +
      labs(x = bquote(.(as.name(hyperPar[i]))), y = bquote('p')) +
      ggtitle("") +
      theme_bw() +
      theme(legend.title=element_blank(), legend.position="bottom") +
      scale_colour_manual(values=c('black', 'blue'),
                          labels=c(bquote('p(' * .(as.name(hyperPar[i]))*')'),
                                   bquote('p(' * .(as.name(hyperPar[i])) * '|y)')))

  }

  exPrior::multiplot(g_list[[1]], g_list[[2]], g_list[[3]], cols = 3)

}
