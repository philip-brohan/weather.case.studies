#!/usr/bin/env Rscript

# Plot a single frame of the CET animation.

library(lubridate)
library(grid)
library(getopt)

opt = getopt(c(
  'year',   'd', 2, "integer",
  'month',  'm', 2, "integer",
  'frame',  'f', 2, "integer"
));

if ( is.null(opt$year) )   { stop("Year not specified") }
if ( is.null(opt$month) )  { stop("Month not specified") }
if ( is.null(opt$frame) )  { stop("Frame not specified") }
frames.per.month<-3 # Increase for extra smoothness
if(opt$frame>frames.per.month) stop("Too many frames")

Imagedir<-sprintf("%s/images/CET.flat",Sys.getenv('SCRATCH'))
if(!file.exists(Imagedir)) dir.create(Imagedir,recursive=TRUE)

# Read in the data
cet<-read.table('../cetml1659on.dat',skip=7,na.strings='-99.9')
cet.total.mean<-mean(cet$V14,na.rm=TRUE)
cet.monthly.means<-rep(NA,12)
cet.monthly.lows<-rep(NA,12)
cet.monthly.highs<-rep(NA,12)
for(m in seq(1,12)) {
  cet.monthly.means[m]<-mean(cet[,m+1],na.rm=TRUE)
  cet.monthly.lows[m]<-quantile(cet[,m+1],probs=0.025,na.rm=TRUE)
  cet.monthly.highs[m]<-quantile(cet[,m+1],probs=0.975,na.rm=TRUE)
}

# Page geometry
page.width<-1080*2
page.height<-1080

pitch=0.16 # 6 spirals in the page height
step<-0.005 # 200 months to the page width

start.x<-0.5*page.width
start.y<-1.0*page.height

y.scale<-60 # page height equivalent to 100C

y.from.month<-function(year,month) {
  return((cet[year-1658,month+1]-cet.monthly.means[month])/y.scale)
}

# Call this function for each month from Jan 1659 to end date
plot.pair<-function(year,month) {

   months.offset<-(opt$year-year)*12+opt$month-month
   y.offset<-(months.offset+(opt$frame-1)/frames.per.month)*pitch*step
   y.npc<-1.0-y.offset
   if(y.npc<0) return() # Off bottom of page

   x.offset<-(months.offset+(opt$frame-1)/frames.per.month)*step
   x.npc<-0.5-x.offset
   while(x.npc<0) x.npc<-x.npc+1
   
   # Show normal line
   gp.grey<-gpar(col=rgb(0.5,0.5,0.5,1),fill=rgb(0.5,0.5,0.5,1),lwd=1)
   grid.lines(x=unit(c(x.npc,x.npc+step),'npc'),
              y=unit(c(y.npc,y.npc+pitch*step),'npc'),
              gp=gp.grey)
   if(x.npc+step>1) {
       grid.lines(x=unit(c(x.npc-1,x.npc-1+step),'npc'),
                  y=unit(c(y.npc,y.npc+pitch*step),'npc'),
                  gp=gp.grey)
   }

   # Show data
   gp<-gpar(col=rgb(0,0,0,1),fill=rgb(0,0,0,1),lwd=2)
   next.month<-ymd(sprintf("%04d-%02d-15",year,month))+months(1)
   grid.lines(x=unit(c(x.npc,x.npc+step),'npc'),
              y=unit(c(y.npc+y.from.month(year,month),
                       y.npc+pitch*step+y.from.month(year(next.month),month(next.month))),
                       'npc'),
              gp=gp)
   if(x.npc+step>1) {
      grid.lines(x=unit(c(x.npc-1,x.npc-1+step),'npc'),
                 y=unit(c(y.npc+y.from.month(year,month),
                       y.npc+pitch*step+y.from.month(year(next.month),month(next.month))),
                       'npc'),
                 gp=gp)
   }
   if(cet[year-1658,month+1]<cet.monthly.lows[month]) {
     gp<-gpar(col=rgb(0,0,1,1),fill=rgb(0,0,1,1),lwd=2)
   }
   if(cet[year-1658,month+1]>cet.monthly.highs[month]) {
     gp<-gpar(col=rgb(1,0,0,1),fill=rgb(1,0,0,1),lwd=2)
   }
   grid.points(x=unit(c(x.npc),'npc'),
               y=unit(c(y.npc+y.from.month(year,month)),'npc'),
               size=unit(0.005,'npc'),
               pch=21,
               gp=gp)
   return()  
 }

plot.to.month<-function(year,month,frame) {

  image.name<-sprintf("%04d-%02d.%02d.png",year,month,frame)
  ifile.name<-sprintf("%s/%s",Imagedir,image.name)
  if(file.exists(ifile.name) && file.info(ifile.name)$size>0) return()

  png(ifile.name,
             width=1080*2,
             height=1080,
             bg='ivory',
             pointsize=24,
             type='cairo')

  while(year>1658) {
    plot.pair(year,month)
    month<-month-1
    if(month==0) {
      year<-year-1
      if(year<1659) next
      month<-12
    }
  }
  dev.off()
}

plot.to.month(opt$year,opt$month,opt$frame)
