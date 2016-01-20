\name{outlinebarplot}
\alias{outlinebarplot}
\title{
 Draw a board style bar plot outline
}
\description{
 Draw a board style bar plot outline
}
\usage{
outlinebarplot(bars,datavector,...)
}

\arguments{
  \item{bars}{
  Mid points of an existing barplot (the values returned by calling barplot(...))
}
  \item{datavector}{
  A vector of heights for the outlined barplot
}
  \item{...}{
  Other parameter passed to the lines. 
}

}

\details{
  This function can be used to make a "board style" outline of a bar
  plot. 
}

\examples{
data1 <- c(1,2,3,4)
data2 <- c(2,3,4,5)
x11()
bars <- barplot(data1,ylim=c(0,6),names.arg=c("a","b","c","d"))
outlinebarplot(bars,data2,col="red", lty = 2, lwd = 3)
box()

  }

\author{
Ben Miller
}
