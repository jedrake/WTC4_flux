
# Modified from plotrix::hexagon
Hexagon <- function (x, y, xdiam = 1, ydiam=xdiam, center=TRUE, col = NA, border = "black") {
  
  xx <- c(x, x, x + xdiam/2, x + xdiam, x + xdiam,
          x + xdiam/2)
  yy <- c(y + ydiam * 0.125, y + ydiam *
            0.875, y + ydiam * 1.125, y + ydiam * 0.875, y +
            ydiam * 0.125, y - ydiam * 0.125)
  
  if(center){
    xx <- xx - xdiam/2
    yy <- yy - ydiam/2
  }
  
  polygon(xx, yy, col = col, border = border)
}

# Hacky method to get spacing between points.
getdiams <- function(cells){
  cel <- as.data.frame(cells)
  cel$xcount <- with(cel, ave(x, y, FUN=length))
  z <- subset(cel, xcount == max(xcount))
  xdiam <- median(diff(z$x))/2
  
  cel$ycount <- with(cel, ave(y, x, FUN=length))
  z2 <- subset(cel, ycount == max(ycount))
  z3 <- subset(z2,x==min(z2$x))
  ydiam <- median(diff(z3$y))/2
  
  return(list(xdiam=xdiam, ydiam=ydiam))
}
###################################################################################################
