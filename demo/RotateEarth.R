rm(list=ls())
library(RoundAndRound)
# install.packages('rgl')  # The rgl package is required.
library(rgl)

par3d()

p.mp = 29.530589 # Period of Moon phase
p.moon = 27.32166 # Period of the Moon orbit around the Earth
p.earth = 365.256363  # Period of the earth orbit around the sun
ps = orbit.parameter(a=100.00, b=100.00);
pe = orbit.parameter(a=152.10, b=147.10)
pm = orbit.parameter(a=40.5500, b=36.3300)
o.sun = c(-pe$c, 0, 0)  # coordinates of the Sun

# dir.create('Movie3d', showWarnings = F, recursive = T)
id1 <- NULL; id2=id1; id0=id1
# open3d()
ax = cbind(c(0,-pe$c,0), c(0,pe$c,0))
axis0 =c(0, 1, 0*sin(d2r(23.5) ) )
spin0 <- spin3d(c(0, 1, 0), rpm = 0 ) # the scene spinner
spin1 <- spin3d(c(0, 1, 0 ), rpm = 24 ) # the scene spinner --- sun
spin2 <- spin3d(axis0, rpm = 30) # the sprite spinner --- earth
spin3 <- spin3d(c(0, 1, 0), rpm = p.moon ) # the sprite spinner --- moon
spriteid <- NULL

phi1 = 1:365 * 360 / p.earth
phi2 = 1:365 * 360 / p.moon
# plot(sin( d2r( phi1)) ); points(sin(d2r(phi2)))
orb.earth= PCS2CCS(theta= -phi1, a=pe$a, ab=pe$a/pe$b)
orb.moon= PCS2CCS(theta= -phi2 , a=pm$a, ab=pm$a/pm$b) + orb.earth

orb.moon=orb.moon+orb.moon*0.1
cols= gray.colors(15, start=0, end=1)
lim = apply(orb.earth, 2, range)
fps = 30
duration= 365/fps
spriteid=NULL
rr = c(8,5,3) * 10

TD2TD<-function(x, z=0){
  x = rbind(x)
  return(cbind(x[,1], 0, x[,2]) )
}

# clear3d(type = 'shapes') # delete the old sprite
# obj.earth <-spheres3d(c(0,0,0), radius=rr[2], col = "white",
#                       texture = system.file('extdata/earth.png', package = 'RoundAndRound') )
# Arrow3D(len=90, type = "lines", lwd=5)
# view3d( theta = 0, phi = 30)
# stop()

f <- function(time, fps) {
  sp1=NULL;sp2=NULL; sp3=NULL
  xx=par3d(skipRedraw = TRUE) # stops intermediate redraws
  clear3d(type = 'shapes') # delete the old sprite
  # message(time)
  readDay = time * fps
  plot3d(orb.moon, type='n', col = 1, ylim=range(orb.moon[,2])+c(-1,1)*20,
         xlim=range(orb.moon[,1])+c(-1,1)*120,
         axes=F, xlab='', ylab='', zlab='');
         # xlab='Xxxxxxxxx', ylab='Yyyyyyyy', zlab='Zzzzzzzz')
  # axes=F, xlab='', ylab='', zlab='');
  # bg3d('black')
  bg3d(texture = system.file("extdata/MilkyWay.png", package = "RoundAndRound"), col = "white")
  # rgl.bg(fogtype = "linear", texture = system.file('extdata/sun.png', package = 'RoundAndRound') )
  aspect3d("iso")
  # text3d(c(50,0,50), texts='Solar System', col='gold', cex=3)
  text3d(c(50,0,50), texts=paste(round(readDay), 'day'), col='gold', cex=2)

  Arrow3D(len=60, type = "lines", lwd=5)
  clear3d(type = "lights")
  light3d(x = cbind(50,25,30),viewpoint.rel = FALSE)
  view3d(theta = 35, phi=15)
  par3d(zoom=0.10)
  # lines3d(cbind(lim[,1], 0,0), col=3)
  # icol=round(abs( (readDay) %% p.mp - p.mp/2)+1) # for Moon Phase
  obj.earth <-spheres3d(c(0,0,0), radius=rr[2], col = "white",
                        texture = system.file('extdata/earth.png', package = 'RoundAndRound') )
  rgl.pop(id = sp1 )
  loc1 = rep(0,3)

  sp1 <<- rgl.sprites(loc1, shape=obj.earth, userMatrix = spin2(time)$userMatrix )
  par3d(skipRedraw = FALSE)
  # rin = readline("GO ON?")
  # if(grepl('n', rin)){
  #   stop()
  # }
}
# if (!rgl.useNULL())
#   play3d(f, duration = duration, startTime = 0)
for(time in 1:(24 * 5) / 24){
  # message(time)
  f(time, fps=1)
  # stop()
}
par3d(skipRedraw = FALSE)
