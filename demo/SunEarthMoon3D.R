rm(list=ls())
library(RoundAndRound)
library(rgl)
par3d()

p.mp = 29.530589
p.moon = 27.32166
p.earth = 365.256363
ps = orbit.parameter(a=100.00, b=100.00);
pe = orbit.parameter(a=152.10, b=147.10)
pm = orbit.parameter(a=40.5500, b=36.3300)
o.sun = c(-pe$c, 0, 0)


dir.create('Movie3d', showWarnings = F, recursive = T)
id1 <- NULL; id2=id1; id0=id1
# open3d()
ax = cbind(c(0,-pe$c,0), c(0,pe$c,0))
spin0 <- spin3d(c(0,1,0), rpm = 0 ) # the scene spinner
spin1 <- spin3d(c(0,.1,0), rpm = 24 ) # the scene spinner --- sun
spin2 <- spin3d(c(0,1,0), rpm = 1) # the sprite spinner --- earth
spin3 <- spin3d(c(0,1,0), rpm = p.moon ) # the sprite spinner --- moon
spriteid <- NULL

orb.earth= PCS2CCS(theta= 0:360, a=pe$a, ab=pe$a/pe$b)
orb.moon= orb.earth+PCS2CCS(theta= 0:360, a=pm$a, ab=pm$a/pm$b)

orb.moon=orb.moon+orb.moon*0.1
cols= gray.colors(15, start=0, end=1)
lim = apply(orb.earth, 2, range)
speed = 50
duration= 136.5/speed
spriteid=NULL
rr = c(3,2,1) * 10

track1=c(PCS2CCS(1:duration * 360 / p.earth, a = pe$a, ab=pe$a/pe$b), 0)
track2=c(PCS2CCS(1:duration * 360 / p.moon, a = pm$a, ab=pm$a/pm$b), 0)
TD2TD<-function(x, z=0){
  return(cbind(x[,1], 0, x[,2]) )
}
f <- function(time) {
  xx=par3d(skipRedraw = TRUE) # stops intermediate redraws
  # on.exit(par3d(skipRedraw = FALSE)) # redraw at the end
  # message(time)
  tday = time * speed
  clear3d(type = 'shapes') # delete the old sprite
  plot3d(orb.moon, type='n', col = 1, ylim=range(orb.moon[,2])+c(-1,1)*20,
         xlim=range(orb.moon[,1])+c(-1,1)*20, xlab='XXX', ylab='ylab', zlab='zlab')
  #, axes=F, xlab='', ylab='', zlab='')
  aspect3d("iso")
  # text3d(c(50,0,50), texts='Solar System', col='gold', cex=3)
  text3d(c(50,0,50), texts=paste(round(tday), 'day'), col='gold', cex=2)
  # bg3d(texture = 'fig/iss006e40544.png')
  # par3d(zoom=0.20)
  # spheres3d(term$xyz,radius = 4, col=2)
  # rgl.texts(term$xyz2, text = term$name, col='white')
  lines3d(TD2TD(orb.earth, 0), col=1)
  lines3d(cbind(o.sun[1], lim[,2], 0), col=2)
  lines3d(cbind(0, lim[,2], 0), col=2)
  lines3d(cbind(lim[,1], 0,0), col=3)
  # lines3d(cbind(0,0, lim[,3]), col=3)
  # cubeid <- shade3d(cube3d(), col = "red")

  loc1=PCS2CCS(-tday * 360 / p.earth, a = pe$a, ab=pe$a/pe$b)
  loc2=PCS2CCS(-tday * 360 / p.moon, a = pm$a, ab=pm$a/pm$b)
  icol=round(abs( (tday) %% p.mp - p.mp/2)+1)
  loc1 = c(loc1[,1], 0, loc1[,2])
  loc2 = c(loc2[,1], 0, loc2[,2])
  obj.sun <- spheres3d(c(0,0,0), radius=50, col='transparent',
                       texture = system.file('extdata/sun.png', package = 'RoundAndRound') )
  obj.earth <-spheres3d(c(0,0,0), radius=20, col = "white",
                        texture = system.file('extdata/earth.png', package = 'RoundAndRound') )
  obj.moon <-spheres3d(c(0,0,0),radius=15, col = cols[icol],
                       texture = system.file('extdata/moon.png', package = 'RoundAndRound') )
  # dev = rgl.cur()
  # subscene = par3d("listeners", dev = dev)
  # M <- par3d("userMatrix", dev = dev, subscene = subscene)
  # matx = rotate3d(obj=M,  angle = time * 10 * pi/30, 0,1,0) # got rotation matrix
  # spriteid <<- rgl.sprites(o.sun, shape=obj.sun, userMatrix = matx)
  sp1 <<- rgl.sprites(o.sun, shape=obj.sun, userMatrix = spin1(time)$userMatrix )
  sp2 <<- rgl.sprites(loc1, shape=obj.earth, userMatrix = spin2(time)$userMatrix )
  sp3 <<- rgl.sprites(loc2+loc1, shape=obj.moon, userMatrix = spin3(time)$userMatrix )
  # spin1(time)
  # spin0(time)
  par3d(skipRedraw = FALSE)
}
if (!rgl.useNULL())
  play3d(f, duration = duration, startTime = 0)

par3d(skipRedraw = FALSE)
