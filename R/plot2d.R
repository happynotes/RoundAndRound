#' Plot in polar coordinate system
#' @param a Radius of start and end points of the arrow.
#' @param theta Angle in polar coordinate system
#' @param orig Origin
#' @param ab Semi-major over semi-minor. ab=1 for a Ring.
#' @param fun Plot function. default = plot
#' @param ... More options in plot function
#' @export
#' @examples
#' \dontrun{
#' n=50
#' par(mfrow=c(2,1))
#' plotpcs(theta = 1:n * 15, a=1:n/10, ab=1, type='l', asp=1)
#' plotpcs(theta = 1:n * 10, a=1:n/10, ab=1, type='l', asp=1)
#' xy = PCS2CCS(theta = 1:n * 10, a=1:n/10, ab=1)
#' xy[,1]=xy[,1]+1
#' points(xy, pch=19, col=terrain.colors(nrow(xy)))
#' }
plotpcs <- function(theta, a, ab=1, orig = c(0,0), fun=graphics::plot, ...){
  # if(is.matrix(theta) ||is.data.frame(theta) ||is.matrix(a) ||is.data.frame(a)){
  #     theta = cbind(theta)
  #     a=cbind(theta)
  #     nr = min(ncol(theta), ncol(a))
  #     xy=NULL
  #     for(i in 1:nr){
  #       xy0 = PCS2CCS(theta[,i], a = a[,i], ab=ab, orig=orig)
  #       xy = rbind(xy, xy0)
  #     }
  # }else{
  xy =PCS2CCS(theta, a = a, ab=ab, orig=orig)
  fun(xy, ...)
}

#' Add arrows in Polar Coordinate System
#' @param r1,r2 Radius of start and end points of the arrow.
#' @param theta Angle in polar coordinate system
#' @param o1,o2 Origin
#' @param ab1,ab2 Semi-major over semi-minor. ab=1 for a Ring.
#' @param ... Mor options for graphics::arrows function.
#' @export
#' @examples
#' \dontrun{
#' x1=PCS2CCS(a=10, ab=1.5)
#' c1 = ab2c(a=10, ab=1.5)
#' plot(x1, type='n', xlim=c(-10,10), ylim=c(-10,10), asp=1)
#' abline(h=0, v=0, asp=1, lty=2)
#' graphics::lines(x1, col=2);
#' points(c1, 0, col=2) # focus
#' Arrow.pcs(theta = 1:12 * 30, r1=0, r2=10, ab1=1.5, length=.1, col=2, o1 = c(c1,0), o2=c(0,0))
#' }
Arrow.pcs <- function(theta,
                      r1=0, r2=1e6,
                      o1=c(0,0), o2 = o1,
                      ab1=1, ab2=ab1,
                      ...){
  o1=rbind(o1)
  o2=rbind(o2)
  c1=PCS2CCS(theta, r1, orig=o1, ab=ab1)
  c2=PCS2CCS(theta, r2, orig=o2, ab=ab2)
  graphics::arrows(c1[,1], c1[,2], c2[,1], c2[,2], ...)
}

#' Plot a planet
#' @param orig Origin
#' @param rad Radius of the planet
#' @param theta Angle of the Arrow inside of the planet
#' @param fun  Function to plot the planet
#' @param cols Color of planet and arrow.
#' @param ab Semi-major over semi-minor. ab=1 for the planet
#' @param arrow Whether plot the arrow.
#' @param arrow.len Length in arrow function.
#' @param ... More options in plot function.
#' @export
#' @examples
#' \dontrun{
#' a = 10;
#' ab=1.5
#' x1=PCS2CCS(a=a, ab=ab)
#' c1 = ab2c(a=a, ab=ab)
#' plot(x1, type='l', xlim=c(-10,10), ylim=c(-10,10), asp=1, col='gray')
#' Arrow.pcs(theta = 1:12 * 30, r1=0, r2=a, ab1=ab, length=.1, col=2, o1 = c(c1,0), o2=c(0,0))
#' pos = PCS2CCS(theta = 1:12 * 30, a=a, ab=ab)
#' plotplanet(orig = pos, arrow.len=0.1)
#' }
plotplanet <- function(orig =c(0,0),
                        rad=1, theta=0, fun=graphics::lines, cols='gray', ab=1,
                        arrow=TRUE, arrow.len=0.1,...){
  cols=matrix(cols, nrow=2)
  orig = rbind(orig)
  nr = nrow(orig)
  tts=matrix(theta, nrow=nr, ncol=1)
  for(i in 1:nrow(orig)){
    o = orig[i,]
    plotpcs(theta = theta+0:360, a = rad, ab=ab, orig = o, fun=fun, col=cols[1])
    # ret=add.ring(orig =o, r=rad, fun=fun, col=cols[1], ...)
    if(arrow) {
      Arrow.pcs(theta = tts[i], r1=-rad, r2=rad, ab1=ab, ab2=ab,
              o1 = o, o2=o, col=cols[2], length = arrow.len)
    }
    # if(arrow) myarrow(theta=theta, orig =o, r1=-rad, r2=rad, col=cols[2], ab=ab, length=arrow.len)
  }
}
