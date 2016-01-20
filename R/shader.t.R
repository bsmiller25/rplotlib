shader.t <-
function(data.df, timevar = "dates", yvar = "y", col = "red"){

  x = data.df[,timevar]
  y = data.df[,yvar]
  
  coord.x = c(x[1], x, x[length(x)])
  
  coord.y = c(0, y, 0)

  polygon(coord.x, coord.y, col = col)

}
