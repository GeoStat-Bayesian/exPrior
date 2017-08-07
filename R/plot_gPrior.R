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


  # Construct the plots
  # =============================================================================
  # To plot both pdf and histogram of measurements in the same plot with dual y-axes,
  # we need to:
  # 1. Plot each plot in separate panel
  # 2. Combine two plots together
  # =============================================================================

  # first define the range for the x-axis,
  # so that it is similar in both overlaid plots
  xrange <- range(df_gPrior$theta)

  # gPrior pdf
  p1 <-
    ggplot(df_gPrior, aes(theta, value, color = variable))+
    geom_line()+
    scale_colour_hue(h = 15+c(180,0),labels=c(expression(f['Y']('Y')),
                                              expression(f['Y']('Y|D')))) +
    labs(x = 'Y', y=NULL) +
    scale_x_continuous(limits = xrange) +
    theme(axis.text.y = element_text(colour="#68382C", size = 14),
          axis.text.x = element_text(size = 13),
          axis.title = element_text(size = 14, face = "bold"),
          text = element_text(),
          legend.title=element_blank(),
          legend.background = element_rect(colour = "black"))
  # gPrior histogram
  p2 <- ggplot(res_gPrior$meas, aes(val, fill = site_id))+
    geom_histogram(alpha = .5)+
    scale_fill_discrete("Site") +
    labs(x = 'Y', y=NULL) +
    # set x axis same as x axis from p1
    scale_x_continuous(limits = xrange) +
    theme( axis.text.y = element_text(colour="#00A4E6", size=14),
           axis.text.x = element_text(size = 13),
           panel.background = element_blank(),
           panel.grid.minor = element_blank(),
           panel.grid.major = element_blank(),
           text = element_text(),
           legend.background = element_rect(colour = "black"))


  # combine two plots
  # =======================================================================
  # To combine two plots, we need to access the inner working of ggplots object.
  # Use gtable package to make gtable objects from ggplot objects
  # gtable object shows how grobs(grid graphical object) are put together to form a ggplot
  # =======================================================================

  # Get the plot grobs
  g1 <- ggplotGrob(p1)
  g2 <- ggplotGrob(p2)

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
    grob$children[[1]]$x <- unit(1, "npc") - grob$children[[1]]$x
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
    plyr::defaults(p$theme, theme_get())
  }

  tml <- plot_theme(p1)$axis.ticks.length   # Tick mark length
  ticks$grobs[[1]]$x <- ticks$grobs[[1]]$x - unit(1, "npc") + tml

  # Fourth, swap margins and fix justifications for the tick mark labels
  ticks$grobs[[2]] <- hinvert_title_grob(ticks$grobs[[2]])

  # Fifth, put ticks back into yaxis
  yaxis$children[[2]] <- ticks

  # Put the transformed yaxis on the right side of g
  g <- gtable_add_cols(g, g2$widths[g2$layout[index, ]$l], pp$r)
  g <- gtable_add_grob(g, yaxis, pp$t, pp$r + 1, pp$b, pp$r + 1,
                       clip = "off", name = "axis-r")

  # Labels grob
  left = textGrob(expression(f['Y']('Y')), x = 0, y = 0.6,
                  just = c("left", "top"),
                  gp = gpar(fontsize = 14, col =  "#68382C"))
  right =  textGrob("Count", x = 1, y = 0.50,
                    just = c("right", "top"),
                    gp = gpar(fontsize = 14, col =  "#00a4e6"))
  labs = gTree("Labs", children = gList(left, right))

  # New row in the gtable for labels
  height = unit(3, "grobheight", left)
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
  g$heights[[6]] = unit(0.7, "cm")
  g$widths[[6]] = unit(3.1,"cm")

  if(!plotMeas){  # Plot pdf only
    p1
  }else{
    grid.draw(g)  # Plot both pdf and histograms
  }

}
