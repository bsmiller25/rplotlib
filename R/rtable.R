rtable <-
function(data.mat,
         colhead = T,
         colhead.fmt.cex = 1,
         colhead.fmt.adjx = .5,
         colhead.fmt.adjy = .5,
         colhead.fmt.font = 2,
         rowhead = T,
         rowhead.fmt.cex = 1,
         rowhead.fmt.adj = 0,
         rowhead.fmt.font = 2,
         rowhead.indent = rep(0,nrow(data.mat)),
         colsep = .5,
         rowsep = 1.5,
         cex = 1,
         adj = .5,
         font = 1){

  options(stringsAsFactors = FALSE)
  data.mat <- as.matrix(apply(data.mat,c(1,2),as.character))
  
  # blank plot
  plot(x = c(1:100), y = c(1:100), type="n",axes=TRUE,xlab =
  "",ylab="", bty = "n", tcl = 0, xaxt= "n", yaxt = "n")

  # spacing 

  # indentation of rowheaders
  for(i in 1:length(rowhead.indent)){
    rownames(data.mat)[i] <-
    paste0(paste0(rep(" ",rowhead.indent[i]),collapse =""),rownames(data.mat)[i])
  }
  # widths
  if(rowhead){
    # row headers width
    par("cex" = rowhead.fmt.cex)
    rowheadmaxwidth <- max(strwidth(rownames(data.mat)))

    # rest of table widths
    par("cex" = cex)
    tablecolmaxwidth <- max(apply(rbind(colnames(data.mat),data.mat), 2, function(x) max(strwidth(x))))

    # combine rowhead colwidth with equal size columns for the rest of
    # the table
    colwidths <- c(rown = rowheadmaxwidth, rep(tablecolmaxwidth ,ncol(data.mat)))

    # add buffer
    colbuffer <- colsep * min(colwidths)

    colwidths <- colwidths + colbuffer
    colwidths[1] <- colwidths[1] * 1.2 
  }
  if(!rowhead){
    # table widths
    par("cex" = cex)
    tablecolmaxwidth <- max(apply(rbind(colnames(data.mat),data.mat), 2, function(x) max(strwidth(x))))

    # combine rowhead colwidth with equal size columns for the rest of
    # the table
    colwidths <- c(rown = rep(tablecolmaxwidth ,ncol(data.mat)))

    # add buffer
    colbuffer <- colsep * min(colwidths)
    colwidths <- colwidths + colbuffer
  }

  # heights
  if(colhead){
    # col headers height
    par("cex" = colhead.fmt.cex)
    colheadmaxheight <- max(strheight(colnames(data.mat)))

    # rest of table heights
    par("cex" = cex)
    tablerowmaxheight <- max(apply(data.mat, 1, function(x) max(strheight(x))))
    
    # combine
    rowheights <- c(cht = colheadmaxheight, rep(tablerowmaxheight,nrow(data.mat)))
    
    # add buffer
    rowbuffer <- rowsep * min(rowheights)
    rowheights <- rowheights + rowbuffer
  }

  if(!colhead){

    # table heights
    par("cex" = cex)
    tablerowmaxheight <- max(apply(data.mat, 1, function(x) max(strheight(x))))
    
    # combine
    rowheights <- c(cht =  rep(tablerowmaxheight,nrow(data.mat)))
    
    # add buffer
    rowbuffer <- rowsep * min(rowheights)   
    rowheights <- rowheights + rowbuffer

  } 
  # print table to the device
  # for each row
  for(r in 1:length(rowheights)){
    # if its the first row, go to the top
    if(r == 1){
      yloc <- (par("usr")[4]-par("usr")[3])/2 + sum(rowheights)/2
      }
    # for each column
    for(k in 1:length(colwidths)){
      # if its the first column, go to the left
      if(k == 1){
        xloc <- (par("usr")[2]-par("usr")[1])/2 - sum(colwidths)/2
      }
      # text to write
      if(colhead & rowhead){
        t <- rbind(c("",colnames(data.mat)),cbind(rownames(data.mat),data.mat))[r,k]
      }
      if(colhead & !rowhead){
        t <- rbind(colnames(data.mat), data.mat)[r,k]
      }
      if(!colhead & rowhead){
        t <- cbind(rownames(data.mat), data.mat)[r,k]
      }
      if(!colhead & !rowhead){
        t <- data.mat[r,k]
      }
      # formatting
      on.colhead <- FALSE
      # first column with header  
      if(k == 1 & rowhead){
        par(cex = rowhead.fmt.cex, adj = rowhead.fmt.adj, font =
        rowhead.fmt.font)
      }
      # first row with header
      if(r == 1 & colhead){
        par(cex = colhead.fmt.cex, font = colhead.fmt.font)
        on.colhead = TRUE
      }
    '  # if first row and first column both with headers, be a col header
      if(k == 1 & r == 1 & colhead & rowhead){
        par(cex = rowhead.fmt.cex, font = rowhead.fmt.font)
        on.colhead = TRUE
      }'
      # most of the table
      if((k != 1 & r != 1) |
         (k != 1 & r == 1 & !colhead) |
         (k == 1 & r != 1 & !rowhead)
         ){
        par(cex = cex, adj = adj, font = font)
      }
      # write the text
      if(!on.colhead){
        text(t, x = xloc , y = yloc)
      }
      if(on.colhead){
        text(t, x = xloc , y = yloc, adj = c(colhead.fmt.adjx, colhead.fmt.adjy))
      }
      xloc <- xloc + colwidths[k]
      
    } # end col loop
    yloc <- yloc - rowheights[r]
  } # end row loop
  options(stringsAsFactors = TRUE)
}
