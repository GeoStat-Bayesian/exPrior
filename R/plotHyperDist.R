#'plot prior and posterior distribution of hyperparameters
#'
#'\code{plotHyperDist} plot prior and posterior distribution of hyperparameters
#'
#'@param resExPrior output from the genExPrior function
#'@return a plot
#'@examples
#'\donttest{
#'exdata <- data.frame(val=c(c(2,3,4),c(2,1),c(6,7,2,3)),
#'                   site_id=c(rep("a",3),rep("b",2),rep("c",4)),
#'                   x = c(c(2,3,4),c(2,3),c(2,2,3,3)),
#'                   y = c(c(2,2,3),c(3,2),c(2,3,2,3)))
#'ex_prior <- genExPrior(exdata=exdata,theta=seq(from=-5,to=5,by=1))
#'plotHyperDist(ex_prior)
#'}
#'@import ggplot2
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
      ggplot2::ggplot(d_i_long, ggplot2::aes(x=x, y=value, color=variable)) +
      ggplot2::geom_line() +
      ggplot2::labs(x = bquote(.(as.name(hyperPar[i]))), y = bquote('p')) +
      ggplot2::ggtitle("") +
      ggplot2::theme_bw() +
      ggplot2::theme_set(theme_classic(base_size = 22)) +
      ggplot2::theme(legend.title=ggplot2::element_blank(), legend.position="bottom") +
      ggplot2::scale_colour_manual(values=c('black', 'blue'),
                          labels=c(bquote('p(' * .(as.name(hyperPar[i]))*')'),
                                   bquote('p(' * .(as.name(hyperPar[i])) * '|y)')))

  }

  multiplot(g_list[[1]], g_list[[2]], g_list[[3]], cols = 3)

}
