library(xts)
library(RoundAndRound)
library(lunar)
rm(list=ls())
source('functions.R')
CN=T
dir.png = 'DemoOut/RingCalender'
dir.create(dir.png, showWarnings = FALSE, recursive = TRUE)
days = seq.Date(from=as.Date('2020-01-01'),
                to=as.Date('2020-12-31'), by='days')

go.plot <- function(days, fn='Calender', CN=FALSE, PDF=TRUE,
                    rotate=FALSE){
  # days = as.Date('2020-01-10')+0:60
  col.mon = colorspace::heat_hcl(n=12, alpha=.4, l = c(80,50))
  col.week = colorspace::terrain_hcl(n=7, alpha=.28, l = c(10,80), c(50,100))
  col.week[c(1,7)]='lightpink'

  col.week = c('lightpink3', colorspace::sequential_hcl(5, alpha=.5), 'lightpink')
  ndays.in.ring = 28

  if(CN){
    week.name=c('星期一', '星期二', '星期三', '星期四',
                '星期五', '星期六', '星期日')
    mon.name=c('一', '二', '三', '四', '五', '六',
               '七', '八', '九', '十', '十一', '十二')
  }else{
    week.name=c('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat')
    mon.name=month.abb
  }
  year = as.numeric(strftime(days[1], '%Y'))


  xdf=builddata(days=days, ndays.in.ring=ndays.in.ring,
                mon.name=mon.name, rotate=rotate)
  xy = xdf[, c('X', 'Y')]
  prefix=file.path(dir.png, ifelse(CN, 'CN', 'EN'))
  dir.create(prefix, showWarnings = FALSE, recursive = TRUE)
  if(PDF){
    filename = file.path(prefix,
                         paste0(fn, format(min(days), '%Y%j'), '.PDF'))
    if(CN){
    pdf(file=filename, family="GB1")
    }else{
      pdf(file=filename)
    }
    par(mar=rep(0,4))
  }else{
    filename = file.path(prefix,
                         paste0(fn, format(min(days), '%Y%j'), '.png'))

    png(filename =filename, width = 10, height =10, res=288, units = 'in')
    if(CN){
      par(mar=rep(0,4), family='Kaiti SC')
    }else{
      par(mar=rep(0,4))
    }
  }
  plot(type='n', xy, asp=1, xlim=range(xdf$X)+c(-1,1)*1.5, axes = FALSE)
  # plot(p.day, type='l', col='lightpink')
  # plot.arrow(xdf, cols = col.week, ndays.in.ring=ndays.in.ring)
  plot.week(xdf=xdf, ndays.in.ring = ndays.in.ring, cols=col.week,week.name=week.name,
            rot=2)
  plotpcs(1:360, a=min(xdf$R)*.8, fun = polygon, col='gold', border='transparent')
  plot.month(xdf, cols = col.mon)
  plot.moon(xdf)
  text(0,0, labels = year, font=2, cex=1.5, col='blue')
  dev.off()
}
go.plot(days, CN=F, PDF=F, rotate=FALSE, fn='EN')
go.plot(days, CN=T, PDF=F, rotate=FALSE, fn='CN')
go.plot(days, CN=F, PDF=T, rotate=FALSE, fn='EN')
go.plot(days, CN=T, PDF=T, rotate=FALSE, fn='CN')

# for(i in 310:400){
#   message(i)
#   go.plot(days+i, CN=T, PDF=F, rotate=TRUE)
# }

# ffmpeg -r 24 -pattern_type glob -i 'CN/*.png'  -pix_fmt yuv420p -loop 1  CN_output_fps24.mp4 -y
# ffmpeg -r 12 -pattern_type glob -i 'CN/*.png'  -pix_fmt yuv420p -loop 1  CN_output_fps12.mp4 -y

# go.plot(days, CN=T, PDF=T)
# go.plot(days, CN=F, PDF=F)
# go.plot(days, CN=F, PDF=T)
