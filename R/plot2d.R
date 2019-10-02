#' Plot in polar coordinate system
#' @param a Radius of start and end points of the arrow.
#' @param theta Angle in polar coordinate system
#' @param orig Origin
#' @param ab Semi-major over semi-minor. ab=1 for a Ring.
#' @param fun Plot function. default = plot
#' @param ... More options in plot function
#' @export
#' @examples
#'
#' n=50
#' par(mfrow=c(2,1))
#' plotpcs(theta = 1:n * 15, a=1:n/10, ab=1, type='l', asp=1)
#' plotpcs(theta = 1:n * 10, a=1:n/10, ab=1, type='l', asp=1)
#' xy = PCS2CCS(theta = 1:n * 10, a=1:n/10, ab=1)
#' xy[,1]=xy[,1]+1
#' points(xy, pch=19, col=terrain.colors(nrow(xy)))
#'
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
#' @param ... More options for graphics::arrows function.
#' @export
#' @examples
#'
#' x1=PCS2CCS(a=10, ab=1.5)
#' c1 = ab2c(a=10, ab=1.5)
#' plot(x1, type='n', xlim=c(-10,10), ylim=c(-10,10), asp=1)
#' abline(h=0, v=0, asp=1, lty=2)
#' graphics::lines(x1, col=2);
#' points(c1, 0, col=2) # focus
#' Arrow.pcs(theta = 1:12 * 30, r1=0, r2=10, ab1=1.5, length=.1, col=2, o1 = c(c1,0), o2=c(0,0))
#'
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
#'
#' a = 10;
#' ab=1.5
#' x1=PCS2CCS(a=a, ab=ab)
#' c1 = ab2c(a=a, ab=ab)
#' plot(x1, type='l', xlim=c(-10,10), ylim=c(-10,10), asp=1, col='gray')
#' Arrow.pcs(theta = 1:12 * 30, r1=0, r2=a, ab1=ab, length=.1, col=2, o1 = c(c1,0), o2=c(0,0))
#' pos = PCS2CCS(theta = 1:12 * 30, a=a, ab=ab)
#' plotplanet(orig = pos, arrow.len=0.1)
#'
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



#' Plot a clock
#' @param time vector, c(h, m, s)
#' @param rad Radius of the clock
#' @param angle Angle of the three Arrows, default angles are calculated from the time
#' @param orig Origin of the clock.
#' @param ab Semi-major over semi-minor. ab=1 for the planet
#' @param val The labels on clock edge
#' @param val.arg Arguments for plot values on clock.
#' @param ring.arg Arguments for plot ring on clock.
#' @param arr.arg Arguments for plot three arrows on clock.
#' @param fun.plot Plot function
#' @param add Whether add plot to existing plot.
#' @param ... More options in plot function.
#' @export
#' @examples
#' plot(0, type='n', xlim=c(-1,1)*1.5, ylim=c(-1,1)*1.5, asp=1)
#' plotclock(add=TRUE,fun.plot = lines, orig=c(0.5,0), rad=.25, val=rep('', 60), time=c(NA, NA, as.numeric(format(Sys.time(), '%S'))))
#' plotclock(add=TRUE,fun.plot = lines, orig=c(0, .5), rad=.25, val=rep('', 60), time=c(NA, as.numeric(format(Sys.time(), '%M')), NA) )
#' plotclock(add=TRUE,fun.plot = lines, orig=c(-.5,0), rad=.25, val=rep('', 60), time=c(as.numeric(format(Sys.time(), '%H')), NA,  NA))
#' plotclock(add=T, fun.plot=lines)
plotclock <- function(
  time=c(as.numeric(format(Sys.time(), format='%H')),
         as.numeric(format(Sys.time(), format='%M')) ,
         as.numeric(format(Sys.time(), format='%S')) ),
  rad=1, ab=1,  orig=c(0,0), val=1:12,
  angle=c((time[1] + ifelse(is.na(time[2]), 0, time[2]/60) + ifelse(is.na(time[3]), 0, time[3]/3600) ) * 30,
          (time[2] +  ifelse(is.na(time[3]), 0, time[3]/60) ) * 6,
          time[3] * 6 )*(-1)+90,
  val.arg= list(col='blue', cex=1),
  arr.arg=list(col=c(1,3,2), lwd=ring.arg$lwd/(1:3)*1.5, lty=rep(1,3), arrlen=rep(.1,3),
               length=c(0.5, 0.8, 0.9)*rad),
  ring.arg=list(col='gold', type='l', lwd=4, len.tick=rad*0.05),
  fun.plot=lines, add=F,
  ...){
  cc=PCS2CCS(orig =orig,  ab=ab, a=rad)
  nv=length(val)
  v.theta = -1 * 1:nv * (360/nv) + 90
  c.txt =PCS2CCS(theta = v.theta,  orig =orig,  ab=ab, a=rad*1.2)
  if(add){
    lines(cc, lwd=ring.arg$lwd, col=ring.arg$col, asp=1)
  }else{
    plot(c.txt, type='n', xlab='', ylab='', asp=1, ...)
    lines(cc, lwd=ring.arg$lwd, col=ring.arg$col, asp=1)
  }

  text(c.txt, paste(val), col=val.arg$col, cex=val.arg$cex)

  Arrow.pcs(v.theta,
            col=ring.arg$col,
            r1=rad,
            r2=rad+ring.arg$len.tick,
            o1=orig,
            lwd=ring.arg$lwd,
            length=0)
  for(i in 1:3){
    if(is.na(angle[i]) | is.null(angle[i]) ){
    }else{
      Arrow.pcs(angle[i],
                col=arr.arg$col[i],
                r1=-0.1*rad,
                r2=arr.arg$length[i],
                o1=orig,
                lwd=arr.arg$lwd[i],
                length=arr.arg$arrlen)
    }
  }
}

