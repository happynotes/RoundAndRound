#' Calculate the status of planet
#' \code{Status.planet}
#' @param t Time (day).
#' @param r.orb Radius of the orbit.
#' @param p.orb Orbital Period.
#' @param ab Semi-major over semi-minor. ab=1 for a Ring.
#' @param orig Reference orgin.
#' @return (x,y) in Cartesian Coordinate System.
#' @export
#' @examples
#' tday = seq(0, 365, 30)
#' x=Status.planet(t=tday, p.orb = 365, r.orb=10)
#' plot(PCS2CCS(0:360, a=10), type='l')
#' plotplanet(orig=x[,-1], rad = .51)
#' grid()
Status.planet <- function(t, p.orb, ab=1,
                          r.orb=1, orig=c(0,0)){
  #p.rot = Rotation Period
  #p.orb = Orbital Period
  #rad  = Radium
  dr= 360 / p.orb
  fac = t * dr - 180
  loc = Orbit.location(t=t, p.orb=p.orb, a = r.orb, orig = orig, ab=ab)
  return(cbind(fac, loc))
}

#' Calculate location of a planet
#' \code{Orbit.location}
#' @param t Time (day).
#' @param a Radius or Semi-major of the orbit.
#' @param p.orb Period of the orbit.
#' @param theta angle in PCS.
#' @param ab Semi-major over semi-minor. ab=1 for a Ring.
#' @param orig Reference orgin.
#' @return (x,y) in Cartesian Coordinate System.
#' @export
#' @examples
#'tday = seq(0, 365, 30)
#'x=Orbit.location(t=tday, p.orb = 365, a=10)
#'plot(PCS2CCS(0:360, a=10), type='l')
#'plotplanet(orig=x, rad = .51)
#'grid()
Orbit.location <-function(t, p.orb, a=1, theta=0, orig=c(0,0), ab=1){
  # tc -- days of full cycle in the Orbit.
  if(p.orb <= 0){
    stop('Wrong days of full cycle.')
  }
  dr = 360 / p.orb
  theta = dr * t
  xy=PCS2CCS(theta=theta, a=a, orig=orig, ab=ab)
  return(xy)
}
