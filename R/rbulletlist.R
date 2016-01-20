rbulletlist <-
function(t, level = rep(1,length(t)), xside = 2, bsep
                        =7, lsep = 3, rowsep = 7,cex.txt =
                        rep(1,length(t)), cex = 1,pch
                        =c("o","-","*","."), w = 50,yloc=90, adjh
                        =rep(.5, length(t)), font=rep(1,length(t)),box=FALSE){


  
  par(cex = cex)
  plot(x = c(1:100), y = c(1:100), type="n",axes=TRUE,xlab =
       "",ylab="", tcl = 0, bty = "n", xaxt= "n", yaxt = "n")

  ytop=yloc
  widths <- vector()
  for(r in 1:length(t)){
    if(r == 1){
      yloc = yloc
    }
 
#chg symbol alignment    
    points(x = xside + lsep * level[r], y = yloc, pch = pch[level[r]])
    par(cex = cex.txt[r])

    widths <- c(widths,strwidth(do.call(function(...)
    paste(...,sep="\n"),as.list(strwrap(t[r], width = (w - xside +
    bsep + lsep * level[r]))))) + xside + bsep + lsep * level[r]) + xside
    
    text(do.call(function(...)
    paste(...,sep="\n"),as.list(strwrap(t[r], width = (w - xside + bsep + lsep * level[r])))), x = xside + bsep + lsep * level[r] , y = yloc, adj = c(0,adjh[r]),font=font[r])

    par(cex = cex.txt[r])
    rowheights <- c(cht = strheight(do.call(function(...)
    paste(...,sep="\n"),as.list(strwrap(t[r], width = (w - xside + bsep + lsep * level[r]))))) + rowsep)
    par(cex = cex)
    
    par(cex = cex)
    yloc <- yloc - rowheights
    
  }
if(box){
  rect(xside,yloc,max(widths)+5,ytop+22,lwd=.7)
}
  
}
