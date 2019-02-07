#' Radian to degree
#' @param x Radian
#' @return Degree
#' @export
#' @examples
#' r = (1:100)/100 * 4 * pi
#' d = r2d(r)
#' rr = d2r(d)
#' plot(d, sin(rr));
#' abline(h=0 )
#' abline(v = 360)
r2d <- function(x){
  return (x * 180 / pi)
}
#' Degree to Radian
#' @param x Degree
#' @return Radian
#' @export
#' @examples
#' r = (1:100)/100 * 4 * pi
#' d = r2d(r)
#' rr = d2r(d)
#' plot(d, sin(rr));
#' abline(h=0 )
#' abline(v = 360)
d2r <- function(x){
  return (x * pi / 180)
}
#' Convert Polar Coordinate System to Cartesian Coordinate System.
#' @param theta angle in PCS.
#' @param a Semi-major (Ellipse) or Radium (Ring).
#' @param ab Semi-major over semi-minor. ab=1 for a Ring.
#' @param orig Reference orgin. Default = c(0, 0)
#' @param rotation Rotation of the theta=0
#' @param clockwise Whether clockwise, Default = FALSE
#' @return (x,y) in Cartesian Coordinate System.
#' @export
#' @examples
#' x1=PCS2CCS(a=10, ab=1.5)
#' x2=PCS2CCS(a=9, ab=1.2)
#' c1 = ab2c(a=10, ab=1.5)
#' c2 = ab2c(a=9, ab=1.2)
#'
#' plot(x1, type='n', xlim=c(-10,10), ylim=c(-10,10), asp=1)
#' abline(h=0, v=0, asp=1, lty=2)
#' lines(x1, col=2);
#' points(c1, 0, col=2)
#' lines(x2, col=3);
#' points(c2, 0, col=3)
#'
#' # Test 2
#' x1=PCS2CCS(a=10, ab=1.5, clockwise = FALSE, rotation=0);
#' x2=PCS2CCS(a=8, ab=1.5, clockwise = FALSE, rotation=45);
#' plot(x1, asp=1, col=terrain.colors(nrow(x1)), pch=19)
#' points(x2, asp=1, col=terrain.colors(nrow(x1)))

PCS2CCS <- function(theta = 0:360, a=1, ab = 1, orig=c(0,0), rotation = 0, clockwise=FALSE ){
  o=rbind(orig)
  rot = d2r(rotation)
  theta = (theta - 90)
  if(clockwise){
    theta = d2r(theta)
  }else{
    theta = -d2r(theta)
  }
  x = sin(theta) * a + o[,1]
  y = cos(theta) * a/ab + o[,2]
  xx = x*cos(rot) - y*sin(rot)
  yy = x*sin(rot) + y * cos(rot)
  xy=cbind(xx, yy);
  return(xy)
}
#' Calculate c in Focus (c, 0)
#' @param ab Semi-major over semi-minor. ab=1 for a Ring.
#' @param a Semi-major (Ellipse) or Radium (Ring).
#' @return c in Focus (c, 0)
#' @export
#' @examples
#' x1=PCS2CCS(a=10, ab=1.5)
#' x2=PCS2CCS(a=9, ab=1.2)
#' c1 = ab2c(a=10, ab=1.5)
#' c2 = ab2c(a=9, ab=1.2)
#' plot(x1, type='n', xlim=c(-10,10), ylim=c(-10,10), asp=1)
#' abline(h=0, v=0, asp=1, lty=2)
#' lines(x1, col=2);
#' points(c1, 0, col=2)
#' lines(x2, col=3);
#' points(c2, 0, col=3)
ab2c <- function(a=1, ab){
  b = a / ab
  c=sqrt( abs(a^2-b^2) );
  return(c)
}
