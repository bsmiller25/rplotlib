tbox <-
function(t,x,y,cex=1, xspace = 1, yspace = 1, lwd = 1){
  text(t, x = x, y = y, cex=cex)
  rect(xleft = x - strwidth(t)/xspace, xright = x + strwidth(t)/xspace, ybottom = y
  - strheight(t)/yspace, ytop = y + strheight(t)/yspace,lwd=lwd)
}
