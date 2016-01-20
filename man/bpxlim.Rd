\name{bpxlim}
\alias{bpxlim}
\title{
   Find the xlim of a barplot
}
\description{
 Calculate the user coordinates x limits of a barplot
}
\usage{
bpxlim(bardata, ...)
}

\arguments{
  \item{bardata}{
  The matrix to be plotted by barplot.
}
  \item{...}{
  Other parameters passed to barplot. Only needed if it would change the
  xlim - ie width.
}
}

\details{
  This should be used if you want to place a line chart or scatter plot
  over a barplot with the points centered in the middle of the bars.

  Getting axes to lineup nicely can be tricky with barplot and plot. My
  suggestion is to use yaxs = "i" in plot and set xlim and ylim in both
  plots. See the example below. 
  
  This function writes the barplot to a postscript in order to calculate
  the necessary user coordinates. It then deletes the postscript and
  returns the x1 and x2 values based on usr coordinates post plotting.
  These values should be used as the xlim of both the barplot and
  regular plot.
}

\examples{
  xlimits <- bpxlim(c(1,2,3,4))
  x11()
  bars <- barplot(c(1,2,3,4), xlim = xlimits, ylim = c(0,5), axes = FALSE, names.arg= c("a","b","c","d"))
  axis(2)
  par(new = TRUE)
  # yaxs = "i" sets 
  plot(bars, c(10,20,30,40), xlim = xlimits, ylim = c(0,50), axes = FALSE, xlab = "", ylab = "", yaxs = "i")
  axis(4)
  box()


  }

\author{
Ben Miller
}
