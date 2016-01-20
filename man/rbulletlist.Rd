\name{rbulletlist}
\alias{rbulletlist}
\title{
   Make a bullet list
}
\description{
Make a bulletted list with given values and indentation levels. 
}
\usage{
rbulletlist(t, level = rep(1, length(t)), xside = 2, bsep = 7, lsep = 3, rowsep = 7, cex.txt = rep(1, length(t)), cex = 1, pch = c("o", "-", "*", "."), w = 50, yloc = 90, adjh = rep(0.5, length(t)), font = rep(1, length(t)), box = FALSE)
}

\arguments{
  \item{t}{
     A character vector with values to be printed.
}
  \item{level}{
    A vector of length(t) with corresponding indentation levels for each
    element of t. 
}
  \item{xside}{
    A horizontal shifting parameter.
}
  \item{bsep}{
    Space between bullets and text.
}
  \item{lsep}{
    Space differential for each level. 
}
  \item{rowsep}{
   Vertical space between each row. 
}
  \item{cex.txt}{
   Text size.
}
  \item{cex}{
   Bullet size.
}
  \item{pch}{
   A vector of symbols to use for bullets.  Each symbol represents a
   level of indentation.
}
  \item{w}{
  A width parameter for text controlling text wrapping.
}
  \item{yloc}{
  A starting height.
}
  \item{adjh}{
  Control of justification.
}
  \item{font}{
  A vector of font types. 
}
  \item{box}{
  A boolean value for drawing a box around the list.
}
}
\details{

}

\author{
Ben Miller
}
\note{
Written rapidly to meet a deadline. May need some tweaking. Please send me improvements.
}

\examples{
 bulls <- c("Basketball","Michael Jordan", "Derrick Rose",
 "Animals","Ferdinand")
x11()
rbulletlist(bulls,
            level = c(1,2,2,1,2),
            font = c(2,1,1,2,1))
box()
}
\keyword{ list}
\keyword{ bullet }
\keyword{ bulletlist }

