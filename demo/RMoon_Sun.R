rm(list=ls())
library(RoundAndRound)
leg.position <- function(xlim, ylim, x=.8, y=.9){
  dx=diff(xlim); dy=diff(ylim); x0=xlim[1]; y0=ylim[1]
  x0=xlim[1];
  y0=ylim[1]
  leg.loc=c(x0 + dx * x, y0+dy * y)
}

rad=1000
r.moon=rad*.35
r.earth=rad*.3
p.moon = 27.32166
ob.earth = 22 * rad

sp = 29.530589
p.earth = 365.256363
rlim = 22
ylim=rlim*rad * c(-1,1) * 6/8;
xlim=rlim*rad * c(-1,1);
leg.loc = leg.position(xlim,ylim,.8,.9)
# iplot=TRUE
iplot=FALSE
origin=c(0,0)
ab0 = 152.10 / 147.10 * 1.0 # *1e6
ab.earth = ab0 * 1.5
o.sun = c( ab2c(ob.earth, ab0), 0);

ab.moon = 405500 / 363300

r.moon = 1*rad

graphics.off()
if(iplot) png("Moon_Sun.png", height = 6, width = 8, unit='in', res=200)

days = round(seq(0, 365, sp/6), 0)
nt=length(days)
bg<-function(){
  par(mar=rep(0,4))
  plot(0, xlim=xlim, ylim=ylim, type='n', asp=1);
  # grid()
  # myarrow(phi = 1:24 * 15, r1 = rad, r2=1e9, lwd=.2, lty=2)
  # Line.Radial(c1=o.sun, phi = 360 * days/p.earth, r=ob.earth, lwd=.8, lty=2, col='gold', ab=ab.earth);
  Arrow.pcs(360 * days/p.earth, o1 = o.sun, o2=c(0,0), ab1 = ab.earth, ab2=ab.earth,
            r1=0, r2=ob.earth, col='gold', lwd=.9, lty=2, length=0)
  plotplanet(orig=o.sun,rad=rad*6, arrow = F, fun=polygon, cols ='coral2', border='gold', lwd=5)  # Sun
  plotpcs(theta=0:360, a=ob.earth, ab=ab.earth, fun = lines, lty=2)
  text(o.sun[1], o.sun[2],'Sun', cex=2)
  sm = Status.planet(t=0:365, p.orb = p.earth,
                     ab=ab.earth, r.orb =ob.earth + rad/365 *2,
                     orig = origin)
  track.moon = Status.planet(t=0:365, p.orb = p.moon, r.orb=r.moon, orig = sm[,-1] )
  lines(track.moon[,-1],lwd=1.5, lty=3)
}
bg()
# dev.off()
# stop()
i=1
nday = length(days)
cols.e=terrain.colors(nday)
cols= gray.colors(16, start=0, end=1)
for(i in 1:nday){
  iday = days[i]
  # message('Day = ', iday)
  # Earth
  loc.e = Status.planet(t=iday, p.orb = p.earth,
                     ab=ab.earth, r.orb =ob.earth + rad/365 *2,
                     orig = origin)
  plotplanet(orig=loc.e[,-1], rad = rad*.19,
             theta = loc.e[,1], len=.1, cols = 2, arrow = F, border=2, fun = polygon )
  orig = loc.e[,-1];
  loc.e = Status.planet(t=iday, p.orb = p.earth, r.orb=ob.earth-rad*3,
                     orig = orig, ab=ab.earth)
  text(loc.e[,2], loc.e[,3], paste0(iday), cex=.7, col='royalblue')

  #Moon
  iorb = floor(iday / p.moon)
  r.orb = rad
  icol=round(abs(iday %% sp - sp/2)+1)

  sm = Status.planet(t=iday, p.orb = p.moon,
                     ab=ab.moon, r.orb = r.moon,
                     orig = orig)
  plotplanet(o=sm[,-1], rad = rad*.6, phi = sm[,1],theta = sm[,1],
             len=.1, cols = c(cols[icol], 'gray40'), fun = polygon )
}

plotplanet(r=rad*.5, o=leg.loc+c(0,1)*rad, fun = polygon,  cols = c(cols[7], 'gray20'))
text(leg.loc[1], leg.loc[2]+rad, 'Moon Phase', adj = -.7, cex=0.6)

plotplanet(r=rad*.18, o=leg.loc+c(0,3)*rad, fun = polygon,  cols = 2, arrow=F, border=2)
text(leg.loc[1], leg.loc[2]+3*rad, 'Earth', adj = -.7, cex=0.6)
# mtext(side=1, 'Earth & Moon in a year', line=-2)
if(iplot) dev.off()
