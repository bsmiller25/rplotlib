\name{rtable}
\alias{rtable}
\title{
  Print a table to a device.
}
\description{
  Prints a table to a graphical device such as X11 or a postscript.
}
\usage{
rtable(data.mat colhead = T, colhead.fmt.cex = 1, colhead.fmt.adjx = .5,
colhead.fmt.adjy = .5, colhead.fmt.font = 2, rowhead = T,
rowhead.fmt.cex = 1, rowhead.fmt.adj = 0, rowhead.fmt.font = 2,
rowhead.indent = rep(0, nrow(data)), colsep = .5, rowsep = 1.5, cex = 1,
adj = .5, font = 1)
}

\arguments{
  \item{data.mat}{
    A matrix with dimnames (or rownames and colnames) of
    data to be printed as a table.
  }
  \item{colhead}{
    Boolean value for printing the colnames of the matrix as the column
    header of the table. Default is TRUE.
  }
  \item{colhead.fmt.cex}{
    Column header size. Default is 1.
  }
  \item{colhead.fmt.adjx}{
    Column header horizontal justification.  Default is .5 for centered.
  }
  \item{colhead.fmt.adjy}{
    Column header vertical justification.  Default is .5 for centered.
    Change to 1 to line up the tops of the column headers.
  }
  \item{colhead.fmt.font}{
    Column header font.  Default is 2 for bold.
  } 
  \item{rowhead}{
    Boolean value for printing the rownames of the matrix as the row
    header of the table. Default is TRUE.
  }
  \item{rowhead.fmt.cex}{
    Row header size. Default is 1.
  }
  \item{rowhead.fmt.adj}{
    Row header justification.  Default is 0 for left justified.
  }
  \item{rowhead.fmt.font}{
    Row header font.  Default is 2 for bold.
  }
  \item{rowhead.indent}{
    A vector with an indentation size for each row. Default is all zeros.
  }
  \item{colsep}{
    Horizontal space between each column as a percentage of column
    width. Default is .5 (meaning 50%). 
  }
  \item{rowsep}{
    Vertical space between each row as a percentage of row height.
    Default is 1.5 (meaning 150%).  
  }
  \item{cex}{
    Size for the other elements of the table. Default is 1.
  }
  \item{adj}{
    Justification for the other elements of the table. Default is .5 for centered. 
  }
  \item{font}{
    Font for the rest of the table. Default is 1 for not bold or italic.
    }
}
\details{
  This function takes a matrix and prints it as a table to a device like
  x11 or a postscript file.  The column names of the matrix are the
  column headers of the table and the row names of the matrix are the
  row headers of the table. 
}

\author{
Ben Miller
}
\note{
 Hastily written and adjusted ad hoc. Still a work in progress. Please
 send me your improvements. 
}

\examples{

 x11()
 layout(matrix(c(1,2)))
 par(oma = c(2,2,2,2), mar = c(2,2,2,2))
 # table stuff
 sumstats <- data.frame(a = c(2214,223,230,5,404),
                        b = c(2553,194,202,20,523),
                        c = c(1564,65,78,7,243)
                        )
 rownames(sumstats)=c(
            "Volume",
            "Mean",
            "Median",
            "Min",
            "Max")
 colnames(sumstats) = c("Bank\nA","Bank\nB","Bank\nC")

 rtable(sumstats,
        rowsep = 1,
        colsep = 2)

 title(main = "Finance Stuff")
 mtext("Sources: Data Source", side=1, line=0.5, adj=0, cex=.7)
 box()

 # world series titles
 ws <- data.frame(team = c("AL","Yankees","Athletics","NL","Cardinals","Giants"),
                  wins = c("","27"," 9","","11","8"),
                  last.won = c("","2009", "1989", "", "2011","2014"),
                  stringsAsFactors = F)

 ws <- as.matrix(ws)
 dimnames(ws)[[1]] <- ws[,1]
 ws <- ws[,-1]
 dimnames(ws)[[2]] <- c("Wins","Last Win")

 rtable(ws,
        rowhead.indent = c(0,10,10,0,10,10),
        rowsep = 1
        )
 title(main = "World Series")
 box()

}

\keyword{ table }
