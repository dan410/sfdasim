
#' Plots the observed data from a functional data set
#' 
#' For observed functional data (arranged in a data frame with a column named
#' ID which contains integer values indexing the curves and a column named Time containing
#' values on the x-axis and a column named X for the observed response) this function will
#' produce a plot of the observed data. By default the points for each curve are connected by lines.
#' 
#' @param dat Data frame containing observed functional data. Must have columns "ID", "Time", and "X" (see details)
#' @export
plot_data <- function(dat){
  gg <- ggplot(dat, aes(x=Time, y=X, group=ID))
  gg + geom_line(aes(color=ID)) + 
  theme_bw() + 
  xlab('t') + 
  ylab('Y(t)') +
  theme(legend.position='none', plot.background=element_blank(),
  		panel.grid.major=element_blank(), panel.grid.minor=element_blank())
}
