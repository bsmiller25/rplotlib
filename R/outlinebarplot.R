outlinebarplot <-
function(bars,datavector,lty=5,lwd=1.5,col="black"){
    xloc <- c(rep(0,length(bars)+1))
    for(i in 2:length(xloc)){
      xloc[i] <- bars[i-1]+(bars[i]-bars[i-1])/2
    }
    xloc[1] <- bars[1]-.5
    xloc[length(xloc)] <- bars[length(bars)]+.5
    yloc <- c(datavector,0)
    outlinebarplotdata <- data.frame(xloc,yloc)
    lines(x=outlinebarplotdata$xloc,
          y=outlinebarplotdata$yloc,
          type='s',
          lty=lty,
          lwd=lwd,
          col=col)
    segments(x0 = xloc[1], y0 = 0, y1 = yloc[1],
             lwd=lwd,lty=lty,col=col)
  }
