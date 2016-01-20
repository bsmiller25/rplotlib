\name{rowmatch}
\alias{rowmatch}
\title{
  Subset dataframes based on matched rows
}
\description{
Take a subset of a dataframe that has rows found in a seperate data frame.
}
\usage{
rowmatch(x, y, x.cols = colnames(x), y.cols = x.cols, matches = "keep")
}
\arguments{
  \item{x}{
 A dataframe to subset.
}
  \item{y}{
 A second dataframe to act as a key.
}
  \item{x.cols}{
 The columns of x that we want to match in y. Default is all of them. 
}
  \item{y.cols}{
 The columns in y, the key, that could contain matches of the x.cols.
 The default is x.cols.
}
  \item{matches}{
 What should the new data frame do with matches? The default is to keep
 them. The other option is to drop them by setting matches = "drop".
 Note: order matters when dropping matches.
}
}

\author{
Ben Miller
}

\examples{

 # find the intersection
 x <- data.frame(a = c(1,2,3,4,5), b = c(6,7,8,9,10))
 y <- data.frame(a = c(1,3,5,7), b = c(6,12,10,9))
 rowmatch(x,y)

 # remove deals between brothers
 deals <- data.frame(borrower = c("Jeff","Mark","Lou","Chris"),
                     lender = c("James","Bruce","Joe","Will"),
                     amount = c(100,250,75,200)
                    )
 brothers <- data.frame(brother1 = c("Mark","Bruce","Chris","Will"),
                        brother2 = c("Bruce","Mark","Will","Chris")
                       )
 deals_no_brothers <- rowmatch(deals,
                               brothers,
                               x.cols = c("borrower","lender"),
                               y.cols = c("brother1","brother2"),
                               matches = "drop")


 # remove duplicates
 source_a <- data.frame(year = c(2010:2015), vol = c(50,100,200,300,400,500))
 source_b <- data.frame(year = c(2014:2017), vol = c(400,500,600,700))
 combo <- rbind(source_a, rowmatch(source_b, source_a, matches = "drop"))
 
}

\keyword{ rowmatch }
\keyword{ match }
\keyword{ key }
