rowmatch <-
function(x, y, x.cols = colnames(x), y.cols = x.cols, matches =
           c("keep","drop")){
  matches <- match.arg(matches)

  if("row.key.name" %in% colnames(x) |
     "row.key.name" %in% colnames(y) ){
    stop("row.key.name is reserved. Please remove it from your column names")
  }
  
  x$row.key.name <- do.call(paste, c(x[,c(x.cols)], sep = ".key."))
  y$row.key.name <- do.call(paste, c(y[,c(y.cols)], sep = ".key."))
  if(matches == "keep"){
    new <- x[x$row.key.name %in% y$row.key.name,]
  }
  if(matches == "drop"){
    new <- x[!(x$row.key.name %in% y$row.key.name),]
  }
  return(new[,-c(length(colnames(new)))])
}
