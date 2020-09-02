library(ggplot2)
library(papaja)
library(tidyr)
bubble_plot <- function(x, y, show_counts = "auto") {
  if (show_counts == "auto") {
    show_counts <- is.factor(x) && is.factor(y)
  }
  if (show_counts) {
    plotf <- geom_count
  } else {
    plotf <- geom_point

    x <- as.numeric(x)
    y <- as.numeric(y)
    combined <- c(x, y)

    the_range <- range(combined)
    width_range <- the_range[2] - the_range[1]
    the_min <- the_range[1] - 0.05 * width_range
    the_max <- the_range[2] + 0.05 * width_range
  }
  df <- expand.grid(x = x, y = y)
  the_plot <- ggplot(data = df, aes(x = x, y = y)) +
    plotf() +
    geom_abline() +
    theme_apa()
  if (!show_counts) {
    the_plot <- the_plot + xlim(the_min, the_max) + ylim(the_min, the_max)
  }
  return(the_plot)
}
