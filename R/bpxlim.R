bpxlim <-
function(bardata, ...){
  postscript("bpxlim.tmp.ps")
  barplot(bardata, ...)
  bpxs <- par("usr")[c(1,2)]
  dev.off()
  system("rm bpxlim.tmp.ps")
  return(bpxs)
}
