#' Creates histogram with a green strip underneath showing different classification
#' intervals used in choropleth maps.
#'
#' @param x        Numeric vector
#' @param n        Number of choropleth classes
#' @param style    Classification method: "quantile", "jenks", "equal", "fisher", "sd", ...
#' @param nbins    Number of histogram bars
#' @param labeller Function used to format the break labels
#' @param title    Plot title (defaults to the style name, capitalized)
#' @param strip    Boolean. If TRUE, adds strip and breaks

library(classInt)   

plot_bin_strip <- function(x,
                           n = 6,
                           style = "quantile",
                           nbins = 60,
                           labeller = function(v) paste0("$", formatC(v, format = "f", digits = 0, big.mark = ",")),
                           title = tools::toTitleCase(style),
                           hist_col = adjustcolor("orange", alpha.f = 0.3),
                           ylab = "",
                           strip = TRUE) {
  
  x <- x[is.finite(x)]
  
  # 1. Class breaks -- same algorithm your choropleth would use
  ci   <- classIntervals(x, n = n, style = style)
  brks <- ci$brks
  k    <- length(brks) - 1
  
  # 2. Light-to-dark green, one shade per class
  cols <- colorRampPalette(c("#F7FCF5", "#A1D99B", "#31A354", "#006D2C"))(k)
  
  # 3. Histogram counts (not plotted yet)
  h <- hist(x, breaks = seq(min(x), max(x), length.out = nbins + 1), plot = FALSE)
  ymax    <- max(h$counts) * 1.05
  strip_h <- ymax * 0.07                 # height of the green strip
  
  # 4. Canvas: leave room below y = 0 for the strip
  plot(NA, xlim = range(brks), ylim = c(-strip_h * 1.6, ymax),
       xaxt = "n", bty = "n", xlab = "", ylab = ylab, main = title)
  
  # Histogram bars
  rect(h$breaks[-length(h$breaks)], 0, h$breaks[-1], h$counts,
       col = hist_col, border = "white")
  
  # Add green strip and vertical line breaks
  if(strip == TRUE){
    rect(brks[-length(brks)], -strip_h, brks[-1], 0, col = cols, border = NA)

    segments(brks, -strip_h * 1.6, brks, ymax, col = "grey30")
    text(brks, ymax * 0.03, labels = labeller(brks),
         srt = 90, adj = c(0, -0.4), cex = 0.8, xpd = NA)
  } else {
    ticks <- seq(min(x), max(x), length.out = 8)
    axis(1, at = ticks, labels = round(ticks, 1), line = -0.75)
  }
  
  
  invisible(ci)
}

# ---- Example -----------------------------------------------------------
set.seed(42)
income <- c(rlnorm(3000, meanlog = 10.5, sdlog = 0.35),
            rlnorm(300,  meanlog = 11.2, sdlog = 0.4))

plot_bin_strip(income, n = 6, style = "quantile")
plot_bin_strip(income, n = 5, style = "jenks", title = "Natural breaks (Jenks)")
