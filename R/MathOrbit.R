#' Give the orbit the parameter
#' @param a   Semi-major axis
#' @param b   Semi-minor axis
#' @param ab Semi-major over semi-minor. ab=1 for a Ring.
#' @export
#' @examples
#' orbit.parameter(a=1, b=1.5)
orbit.parameter <- function(a, b=NULL, ab=NULL){
  # a                 # semi-major axis
  if( all(is.null(c(b,ab)) || all(is.na(c(b,ab)))) ){
    stop("b or ab is missing")
  }
  if(is.null(b)){
    b = a / ab
  }
  if(is.null(ab)){
    ab = a / b
  }
  c <- ab2c(a, a/b)              # distance from the center to a focus
  e <- c/a  #eccentricity
  ret = list(a,b,c,e)
  names(ret) = c('a','b','c','e')
  return(ret)
}
