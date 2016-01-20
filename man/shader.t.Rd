\name{shader.t}
\alias{shader.t}
\title{
  Shades a portion of a plot. 
}
\description{
  Shades under a time plot when given dates and values. 
}
\usage{
shader.t(data.df, timevar = "dates", yvar = "y", col = "red")
}

\arguments{
  \item{data.df}{
   A data frame of plotted series. 
}
  \item{timevar}{
   The column in the data frame that is used as date/time. 
}
  \item{yvar}{
   The variable to shade under.
}
  \item{col}{
   The color of the shading. 
}
}
\details{
}

\author{
Ben Miller
}

\examples{

 # create a stacked area chart

 # create some data
 dates <- seq(as.Date("2000-01-01"), as.Date("2010-01-01"), by = "year")
 y <- rnorm(11, mean = 10)
 y2 <- y + rnorm(11, mean = 10)
 df <- data.frame(dates,y, y2)

 # plot it
 x11()
 plot(df$dates,
      df$y2,
      ylim = c(min(df[-1]) * .8, max(df[-1] * 1.2)),
      type = "l",
      xlab = "",
      ylab = "Volume")
 shader.t(df, timevar = "dates", yvar = "y2", col = "blue")
 lines(df$dates,
       df$y)
 shader.t(df, timevar = "dates", yvar = "y", col = "red")

}
\keyword{ shading }
