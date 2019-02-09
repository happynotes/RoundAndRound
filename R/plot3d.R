#' Plot 3D Arrow axis.
#' \code{Arrow}
#' @param len Length of the arrow.
#' @param cols Colors of axis.
#' @param orig Origin of the axis.
#' @param ...  More options of arrow3d().
#' @export
Arrow <- function(len=10, orig=c(0,0,0), cols=c(2:4), ...){
  x = matrix(len, nrow=3, ncol=1)
  cols =matrix(cols, nrow=3, ncol=1)
  arrow3d(orig, c(1,0,0) * x[1], col=cols[1], ...)
  arrow3d(orig, c(0,1,0) * x[2], col=cols[2], ...)
  arrow3d(orig, c(0,0,1) * x[3], col=cols[3], ...)
}
