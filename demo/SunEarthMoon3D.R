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
spin0 <- spin3d(c(0, 1, 0), rpm = 0 ) # the scene spinner
spin1 <- spin3d(c(0, 1, 0 ), rpm = 24 ) # the scene spinner --- sun
spin2 <- spin3d(c(0, 1, 0*sin(d2r(23.5))), rpm = p.mp) # the sprite spinner --- earth
spin3 <- spin3d(c(0, 1, 0), rpm = p.moon ) # the sprite spinner --- moon
spriteid <- NULL

phi1 = 1:365 * 360 / p.earth
phi2 = 1:365 * 360 / p.moon
# plot(sin( d2r( phi1)) ); points(sin(d2r(phi2)))
orb.earth= PCS2CCS(theta= -phi1, a=pe$a, ab=pe$a/pe$b)
orb.moon= PCS2CCS(theta= -phi2 , a=pm$a, ab=pm$a/pm$b) + orb.earth

# loc1=PCS2CCS(-theDay * 360 / p.earth, a = pe$a, ab=pe$a/pe$b)
# loc2=PCS2CCS(-theDay * 360 / p.moon, a = pm$a, ab=pm$a/pm$b)

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
irun =0
f <- function(time, fps) {
  # sp1=NULL;sp2=NULL; sp3=NULL
  xx=par3d(skipRedraw = TRUE) # stops intermediate redraws
  # on.exit(par3d(skipRedraw = FALSE)) # redraw at the end
  theDay = time * fps
  clear3d(type = 'shapes') # delete the old sprite
  plot3d(orb.moon, type='n', col = 1, ylim=range(orb.moon[,2])+c(-1,1)*20,
         xlim=range(orb.moon[,1])+c(-1,1)*120,
         axes=F, xlab='', ylab='', zlab='');
         # xlab='Xxxxxxxxx', ylab='Yyyyyyyy', zlab='Zzzzzzzz')
  # bg3d('black')
  bg3d(texture = system.file("extdata/MilkyWay.png", package = "RoundAndRound"), col = "white")
  # rgl.bg(fogtype = "linear", texture = system.file('extdata/sun.png', package = 'RoundAndRound') )
  aspect3d("iso");irun = irun +1
  if(irun==1){
    view3d( theta = 15, phi = 30)
  }
  text3d(c(50,0,50), texts=paste(round(theDay), 'day'), col='gold', cex=2)
  par3d(zoom=0.30)
  lines3d(TD2TD(orb.earth, 0), col=4, lwd=2)
  lines3d(cbind(o.sun[1], lim[,2], 0), col=2)
  lines3d(cbind(lim[,1], 0,0), col=3)
  # lines3d(TD2TD(orb.moon, 0), col='gold', lwd=1.5)
  # icol=round(abs( (theDay) %% p.mp - p.mp/2)+1) # for Moon Phase
  # Arrow3D(orig=-o.sun,len=rep(1,3)*50, type='lines', lwd=10)
  obj.sun <- spheres3d(c(0,0,0), radius=rr[1], col='transparent', emission = "white",
                       texture = system.file('extdata/sun.png', package = 'RoundAndRound') )
  obj.earth <-spheres3d(c(0,0,0), radius=rr[2], col = "white",
                        texture = system.file('extdata/earth.png', package = 'RoundAndRound') )
  # obj.earth <- shade3d(translate3d(scale3d(cube3d(), 40,40,40),0,0,0),col="green", alpha = 1)
  obj.moon <-spheres3d(c(0,0,0),radius=rr[3], col = "white", #cols[icol],
                       texture = system.file('extdata/moon.png', package = 'RoundAndRound') )
  # rgl.pop(id = c(sp1, sp2, sp3) )
  clear3d(type = "lights")
  light3d(x = rbind(o.sun), diffuse = "gray75", specular = "gray75", viewpoint.rel = FALSE)
  light3d(diffuse = "gray10", specular = "gray25")
  light3d(x = rbind(o.sun), diffuse = "gray75", specular = "gray75", viewpoint.rel = FALSE)
  loc1=PCS2CCS(-theDay / p.earth * 360, a = pe$a, ab=pe$a/pe$b)
  loc2=PCS2CCS(-theDay / p.moon * 360, a = pm$a, ab=pm$a/pm$b) + loc1
  #  loc1 = orb.earth[time, ]
  #  loc2 = orb.moon[time, ]
  loc1 = TD2TD(loc1)
  loc2 = TD2TD(loc2)
  # dev = rgl.cur()
  # subscene = par3d("listeners", dev = dev)
  # M <- par3d("userMatrix", dev = dev, subscene = subscene)
  # matx = rotate3d(obj=M,  angle = time * 10 * pi/30, 0,1,0) # got rotation matrix
  # spriteid <<- rgl.sprites(o.sun, shape=obj.sun, userMatrix = matx)

  sp1 <<- rgl.sprites(o.sun, shape=obj.sun, userMatrix = spin1(time)$userMatrix )
  sp2 <<- rgl.sprites(loc1, shape=obj.earth, userMatrix = spin2(time)$userMatrix )
  sp3 <<- rgl.sprites(loc2, shape=obj.moon, userMatrix = spin3(time)$userMatrix )
  par3d(skipRedraw = FALSE)
  # rin='x'
  # # rin = readline(paste(theDay))
  # if(grepl('n', rin)){
  #   stop()
  # }
}
# if (!rgl.useNULL())
#   play3d(f, duration = duration, startTime = 0, fps=1)
for(time in 1:300){
  f(time, fps=1)
  # stop()
}
par3d(skipRedraw = FALSE)
