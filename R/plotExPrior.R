#'plot ex-situ prior
#'
#'\code{plotExPrior} plot informative and non-informative priors
#'
#'@param resExPrior output from the genExPrior function
#'@param plotExData boolean asking whether to additionaly plot the ex-situ data
#'@return a plot
#'@import ggplot2
#'@import grid
#'@importFrom gtable gtable_add_grob
#'@export
plotExPrior <- function(resExPrior,plotExData=F){

  # construct single dataframe with uninformative and informative priors
  # easier for ggplot2
  df_exPrior <- data.frame(theta = resExPrior$uPrior$x,
                           uPrior = resExPrior$uPrior$y,
                           exPrior = resExPrior$exPrior$y)

  df_exPrior <- reshape2::melt(df_exPrior, id.vars = "theta")


  # Construct the plots
  # =============================================================================
  # To plot both pdf and histogram of measurements in the same plot with dual y-axes,
  # we need to:
  # 1. Plot each plot in separate panel
  # 2. Combine two plots together
  # =============================================================================

  # first define the range for the x-axis,
  # so that it is similar in both overlaid plots
  xrange <- range(df_exPrior$theta)

  # exPrior pdf
  p1 <-
    ggplot2::ggplot(df_exPrior, ggplot2::aes(theta, value, color = variable)) +
    ggplot2::geom_line() +
    ggplot2::scale_colour_manual(values=c('black','blue'), labels=c(expression(paste('p(', theta, ')') ),
                                               expression(paste('p(', theta, '|y)')))) +
    labs(x = expression(theta), y = NULL) +
    ggplot2::scale_x_continuous(limits = xrange) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(colour="#68382C", size = 14),
                   axis.text.x = ggplot2::element_text(size = 13),
                   axis.title = ggplot2::element_text(size = 14, face = "bold"),
                   text = ggplot2::element_text(),
                   legend.title = ggplot2::element_blank(),
                   legend.background = ggplot2::element_rect(colour = "black"))
  # exPrior histogram
  p2 <- ggplot2::ggplot(resExPrior$exdata, ggplot2::aes(val, fill = site_id)) +
    ggplot2::geom_histogram(alpha = .5) +
    ggplot2::scale_fill_discrete("Site") +
    labs(x = 'y', y = NULL) +
    # set x axis same as x axis from p1
    ggplot2::scale_x_continuous(limits = xrange) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(colour="#00A4E6", size=14),
                   axis.text.x = ggplot2::element_text(size = 13),
                   panel.background = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   text = ggplot2::element_text(),
                   legend.background = ggplot2::element_rect(colour = "black"))


  # combine two plots
  # =======================================================================
  # To combine two plots, we need to access the inner working of ggplots object.
  # Use gtable package to make gtable objects from ggplot objects
  # gtable object shows how grobs(grid graphical object) are put together to form a ggplot
  # =======================================================================

  # Get the plot grobs
  g1 <- ggplot2::ggplotGrob(p1)
  g2 <- ggplot2::ggplotGrob(p2)

  # Get the locations of the plot panels in g1.
  pp <- c(subset(g1$layout, name == "panel", se = t:r))

  # Overlap panel for second plot on that of the first plot
  g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, pp$l, pp$b, pp$l)

  # ggplot contains many labels that are themselves complex grob;
  # usually a text grob surrounded by margins.
  # When moving the grobs from, say, the left to the right of a plot,
  # make sure the margins and the justifications are swapped around.
  # The function below does the swapping.
  # Taken from the cowplot package:
  # https://github.com/wilkelab/cowplot/blob/master/R/switch_axis.R
  hinvert_title_grob <- function(grob){

    # Swap the widths
    widths <- grob$widths
    grob$widths[1] <- widths[3]
    grob$widths[3] <- widths[1]
    grob$vp[[1]]$layout$widths[1] <- widths[3]
    grob$vp[[1]]$layout$widths[3] <- widths[1]

    # Fix the justification
    grob$children[[1]]$hjust <- 1 - grob$children[[1]]$hjust
    grob$children[[1]]$vjust <- 1 - grob$children[[1]]$vjust
    grob$children[[1]]$x <- ggplot2::unit(1, "npc") - grob$children[[1]]$x
    grob
  }

  # Get the y axis from g2 (axis line, tick marks, and tick mark labels)
  index <- which(g2$layout$name == "axis-l")  # Which grob
  yaxis <- g2$grobs[[index]]                  # Extract the grob

  # yaxis is a complex of grobs containing the axis line, the tick marks, and the tick mark labels.
  # The relevant grobs are contained in axis$children:
  #   axis$children[[1]] contains the axis line;
  #   axis$children[[2]] contains the tick marks and tick mark labels.

  # Second, swap tick marks and tick mark labels
  ticks <- yaxis$children[[2]]
  ticks$widths <- rev(ticks$widths)
  ticks$grobs <- rev(ticks$grobs)

  # Third, move the tick marks
  # Tick mark lengths can change.
  # A function to get the original tick mark length
  # Taken from the cowplot package:
  # https://github.com/wilkelab/cowplot/blob/master/R/switch_axis.R
  plot_theme <- function(p) {
    plyr::defaults(p$theme, ggplot2::theme_get())
  }

  tml <- plot_theme(p1)$axis.ticks.length   # Tick mark length
  ticks$grobs[[1]]$x <- ticks$grobs[[1]]$x - ggplot2::unit(1, "npc") + tml

  # Fourth, swap margins and fix justifications for the tick mark labels
  ticks$grobs[[2]] <- hinvert_title_grob(ticks$grobs[[2]])

  # Fifth, put ticks back into yaxis
  yaxis$children[[2]] <- ticks

  # Put the transformed yaxis on the right side of g
  g <- gtable_add_cols(g, g2$widths[g2$layout[index, ]$l], pp$r)
  g <- gtable_add_grob(g, yaxis, pp$t, pp$r + 1, pp$b, pp$r + 1, clip = "off",
                       name = "axis-r")

  # Labels grob
  left = textGrob(expression(p), x = 0.02, y = 0.5, just = c("left", "top"),
                  gp = gpar(fontsize = 14, col =  "#68382C"))
  right =  textGrob("Count", x = 1.0, y = 0.5, just = c("right", "top"),
                    gp = gpar(fontsize = 14, col =  "#00a4e6"))
  labs = gTree("Labs", children = gList(left, right))

  # New row in the gtable for labels
  height = ggplot2::unit(3, "grobheight", left)
  g = gtable_add_rows(g, height, 2)

  # Put the label in the new row
  g = gtable_add_grob(g, labs, t=3, l=3, r=5)

  # Turn off clipping in the plot panel
  g$layout[which(g$layout$name == "panel"), ]$clip = "off"

  # ======================================================================
  # Everything looks great except for the missing panel in the 2nd plot.
  # We'll try to fix it by:
  # 1. Find out grobs information of the legend
  # 2. Extract it from 2nd plot and add it to combined plot
  # ======================================================================


  # Extract legend http://stackoverflow.com/questions/35673831/position-a-legend-with-gtable
  leg1 <- g1$grobs[[which(g1$layout$name == "guide-box")]]
  leg2 <- g2$grobs[[which(g2$layout$name == "guide-box")]]

  # row binding two legends so that we can have two legends lay out vertically
  # then replace the legend in the combined plot to display
  g$grobs[[which(g$layout$name == "guide-box")]] <-
    gtable:::rbind_gtable(leg1, leg2, "first")

  # Adjust height and width of panel to fit legends
  g$heights[[6]] = ggplot2::unit(0.7, "cm")
  g$widths[[6]] = ggplot2::unit(3.1,"cm")

  if(!plotExData){  # Plot pdf only
    p1 + ggplot2::theme_bw()
  }else{
    grid.draw(g) + ggplot2::theme_bw()  # Plot both pdf and histograms
  }

}
