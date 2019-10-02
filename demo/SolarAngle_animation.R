library(RoundAndRound)
rm(list=ls())
source('R/Geometry.R')
#https://physics.weber.edu/schroeder/ua/SunAndSeasons.html
o=c(0,0)
R=10
cols=c( 'blue', 1,'red')
alpha=23.5
lat=23.5*1

cn=TRUE
# cn=FALSE
logo='Lele(RoundAndRound)'
ht=12
wd=14
if(cn){
  time.tag =c('夏至','春/秋分','冬至')
  pivot.tag=c('假想天轴','地轴')
  dir4=c('东', '北', '西', '南')
  # png('SolarRadiation_cn.png', height=ht, width=wd, unit='cm', res=200)
  par(mar=rep(0,4), family='STSong')
}else{
  time.tag =c('Summer Equinox','Solstice','Winter Equinox')
  pivot.tag=c('Image Solar Axis', 'Earth Axis')
  dir4=c('East', 'North', 'West', 'South')
  # png('SolarRadiation_en.png', height=ht, width=wd, unit='cm', res=200)
  par(mar=rep(0,4))
}

xlim=c(-1,1)*R + c(-1, 1)*4
ylim=xlim
cc.sun ='gold'
cc.sky='darkred'

tt=.0

d.fun <- function(fun=cos, x){
  fun(x * pi / 180)
}
dcos <-function(x){d.fun(cos, x)}
dsin <-function(x){d.fun(sin, x)}
c.earth=PCS2CCS(a=R, ab=5)
c.dir=PCS2CCS(a=R+2, ab=5, theta=c(90, 200, 270, 340))
c.e4=PCS2CCS(theta=90*c(1+tt,2,3+tt,4), a=R, ab=5)
c.sky=PCS2CCS(a=R, ab=1)
c.me =PCS2CCS(a=1, ab=5)

o1=rotate(PCS2CCS(180, a=dsin(alpha)*R), -lat)
o2=rotate(PCS2CCS(0, a=dsin(alpha)*R), -lat)


c.sol1=PCS2CCS(theta = 1:360, a=R*dcos(alpha-lat),
               ab=dcos(lat-alpha)*100, orig=c(0, 0))
c.sol1=rotate(c.sol1, theta=90, orig=c(0,0))

c.sol2=PCS2CCS(theta = 1:360, a=R*dcos(0-lat),
               ab=dcos(lat-alpha)*100, orig=c(R/dcos(90-lat), 0))
# c.sol2=rotate(c.sol2, theta=90-0, orig=c(R/dcos(90-lat), 0))

c.sol3=PCS2CCS(theta = 1:360,a=R*dcos(alpha), ab=dcos(alpha)*5,  orig = o2)
# c.sol3=rotate(c.sol3, theta=90-lat, orig=o2)

# dev.off()
plot(0, type='n', xlim=xlim, ylim=ylim, asp=1, xlab='', ylab='',
     xaxt='n',yaxt='n')
box( bty = "L")
lines(c.sky, col=cols[1], lwd=3)
grid()
lines(c.earth, col=cols[2])

points(rbind(o1,o2))

lines(c.sol1[, ], col=1, lty=2)
lines(c.sol2[, ], col=2, lty=2)
stop()
# lines(c.sol3[, ], col=3, lty=2)
# id1=1:155+95;
# id2=1:180+82;
# id3=1:205+71;
# lines(c.sol1[id1, ], col=cols[3], lty=2)
# lines(c.sol2[id2, ], col=cols[3], lty=2)
# lines(c.sol3[id3, ], col=cols[3], lty=2)
plotplanet(rad=R, ab=5, fun=polygon, col=rgb(0.3, .8, .3, 0.8)) # earth plane
# lines(c.sol1[-id1, ], col=cols[3], lty=1)
# lines(c.sol2[-id2, ], col=cols[3], lty=1)
# lines(c.sol3[-id3, ], col=cols[3], lty=1)

polygon(c.me, col=cols[2])
points(rbind(o1, o2), col=cc.sky, pch=20);
# Arrow.pcs(theta=90*c(1+tt,2,3+tt,4), r1=0, r2=R, o1=o,ab2=5)
# Imaging Pivot of sky.
Arrow.pcs(theta=180-lat, r1=-1.5*R, r2=1.5*R, lwd=1, col=cc.sky, lty=2)
Arrow.pcs(theta=180, r1=-1.5*R, r2=1.5*R, lwd=1, col='darkgreen', lty=1)

points(c.e4, pch=20, col=cols[2])
stop()
plotplanet(rbind(c.sol1[1,], c.sol2[1,], c.sol3[1,]),
           rad=.5, fun=polygon, col=cc.sun)

mtext(side=3, paste0(lat, '-degree'), cex=2, line=-2)
t3= 270 - c(0:2)*alpha
tmp=lapply(t3, function(x){
  Arrow.pcs(theta=x, r2=-R, lwd=.92, col=cc.sun, length=.1)
})
text(rbind(c.sol1[1,], c.sol2[1,], c.sol3[1,]) + cbind(rep(1,3), 1),
     time.tag)

text(PCS2CCS(theta=170, a=R*1.2), pivot.tag[2], col='darkgreen')
text(PCS2CCS(theta=170-alpha, a=R*1.3), pivot.tag[1], col=cc.sky)
text(c.dir, dir4)
mtext( side=1, line=-3, logo, col=rgb(.2,.2,.2, .15), cex=1)
dev.off()
