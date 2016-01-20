rowmatch <-
function(x, y, x.cols = colnames(x), y.cols = x.cols,
  matches = "keep"){  
  x$rowkeyname7 <- do.call(paste, c(x[,c(x.cols)], sep = ".key."))
  y$rowkeyname7 <- do.call(paste, c(y[,c(y.cols)], sep = ".key."))
  if(matches == "keep"){
    new <- x[x$rowkeyname7 %in% y$rowkeyname7,]
  }
  if(matches == "drop"){
    new <- x[!(x$rowkeyname7 %in% y$rowkeyname7),]
  }
  return(new[,-c(length(colnames(new)))])
}
