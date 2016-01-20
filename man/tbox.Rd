\name{tbox}
\alias{tbox}
\title{
  Print a textbox to a device
}
\description{
Print a textbox to a device like X11 or postscript.  Useful for adding a
textbox to a chart.
}
\usage{
tbox(t, x, y, cex = 1, xspace = 1, yspace = 1, lwd = 1)
}

\arguments{
  \item{t}{
 The text contained in the textbox
}
  \item{x}{
 The X coordinate of the center of the box (user coordinates).
}
  \item{y}{
 The Y coordinate of the center of the box (user coordinates).
}
  \item{cex}{
 Character expansion.
}
  \item{xspace}{
 Horizontal space between the text and the box.
}
  \item{yspace}{
 Vertical space between the text and the box. 
}
  \item{lwd}{
 Line width of the box. 
}
}

\author{
Ben Miller
}

\examples{
 x <- seq(as.Date("2000-01-01"),as.Date("2010-01-01"),by = "year")
 y <- rnorm(11, mean = 10, sd = 2)
 x11()
 plot(x,y,type = 'l', ylim = c(min(y) - 2, max(y) + 2))
 tbox("This is a\n textbox", x = as.Date("2005-01-01"), y = max(y + 1))
}

\keyword{ textbox }
\keyword{ text }
