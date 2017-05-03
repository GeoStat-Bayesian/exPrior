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
  g_list <- list()
  
  for(i in 1:length(hyperPar)){
    
    d_i <- data.frame(x=d_prior[[i]]$x,
                      prior=d_prior[[i]]$y,
                      post=d_post[[i]]$y)
    
    d_i_long <- reshape2::melt(data = d_i,id.vars ="x")
    
    g_list[[i]] <- 
      ggplot(d_i_long, aes(x=x, y=value, color=variable)) +
      geom_line() +
      labs(x = bquote(.(as.name(hyperPar[i]))), 
           y = bquote('f(' * .(as.name(hyperPar[i])) * '|'*theta*'*)')) +
      ggtitle("") +
      theme(legend.title=element_blank(),
            legend.position="bottom") +
      scale_colour_manual(values=c('black','blue'),
                          labels=c(bquote('f(' * .(as.name(hyperPar[i]))*')'),
                                   bquote('f(' * .(as.name(hyperPar[i])) * '|'*theta*'*)'))) 
    
  }
  
  gPrior::multiplot(g_list[[1]],g_list[[2]],g_list[[3]],cols = 3)  
  
}
