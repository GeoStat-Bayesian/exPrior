
#'multiplot function
#'
#'\code{multiplot} allows to combines multiple plots
#'
#'@param ... ggplot objects
#'@param plotlist list of the ggplot objects
#'@param file filename
#'@param cols Number of columns in layout
#'@param layout A matrix specifying the layout. If present, 'cols' is ignored
#'@return a plot
#'@export
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {

  # Multiple plot function
  #
  # from http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_pa ge_(ggplot2)/
  #
  # If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
  # then plot 1 will go in the upper left, 2 will go in the upper right, and
  # 3 will go all the way across the bottom.
  #

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    ggplot2::grid.newpage()
    ggplot2::pushViewport(ggplot2::viewport(layout = ggplot2::grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = ggplot2::viewport(layout.pos.row = matchidx$row,
                                               layout.pos.col = matchidx$col))
    }
  }
}
