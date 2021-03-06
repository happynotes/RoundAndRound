% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot2d.R
\name{plotclock}
\alias{plotclock}
\title{Plot a clock}
\usage{
plotclock(time = c(as.numeric(format(Sys.time(), format = "\%H")),
  as.numeric(format(Sys.time(), format = "\%M")),
  as.numeric(format(Sys.time(), format = "\%S"))), rad = 1, ab = 1,
  orig = c(0, 0), val = 1:12, angle = c((time[1] +
  ifelse(is.na(time[2]), 0, time[2]/60) + ifelse(is.na(time[3]), 0,
  time[3]/3600)) * 30, (time[2] + ifelse(is.na(time[3]), 0, time[3]/60)) *
  6, time[3] * 6) * (-1) + 90, val.arg = list(col = "blue", cex = 1),
  arr.arg = list(col = c(1, 3, 2), lwd = ring.arg$lwd/(1:3) * 1.5, lty =
  rep(1, 3), arrlen = rep(0.1, 3), length = c(0.5, 0.8, 0.9) * rad),
  ring.arg = list(col = "gold", type = "l", lwd = 4, len.tick = rad *
  0.05), fun.plot = lines, add = F, ...)
}
\arguments{
\item{time}{vector, c(h, m, s)}

\item{rad}{Radius of the clock}

\item{ab}{Semi-major over semi-minor. ab=1 for the planet}

\item{orig}{Origin of the clock.}

\item{val}{The labels on clock edge}

\item{angle}{Angle of the three Arrows, default angles are calculated from the time}

\item{val.arg}{Arguments for plot values on clock.}

\item{arr.arg}{Arguments for plot three arrows on clock.}

\item{ring.arg}{Arguments for plot ring on clock.}

\item{fun.plot}{Plot function}

\item{add}{Whether add plot to existing plot.}

\item{...}{More options in plot function.}
}
\description{
Plot a clock
}
\examples{
plot(0, type='n', xlim=c(-1,1)*1.5, ylim=c(-1,1)*1.5, asp=1)
plotclock(add=TRUE,fun.plot = lines, orig=c(0.5,0),
rad=.25, val=rep('', 60), time=c(NA, NA, as.numeric(format(Sys.time(), '\%S'))))
plotclock(add=TRUE,fun.plot = lines, orig=c(0, .5),
rad=.25, val=rep('', 60), time=c(NA, as.numeric(format(Sys.time(), '\%M')), NA) )
plotclock(add=TRUE,fun.plot = lines, orig=c(-.5,0),
rad=.25, val=rep('', 60), time=c(as.numeric(format(Sys.time(), '\%H')), NA,  NA))
plotclock(add=TRUE, fun.plot=lines)
}
