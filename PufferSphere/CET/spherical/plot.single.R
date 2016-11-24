#!/usr/bin/env Rscript

# Plot a single frame of the CET animation.
# Spherical version for puffersphere

library(lubridate)
library(grid)
library(getopt)
library(memoise)

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

Imagedir<-sprintf("%s/images/CET.spherical",Sys.getenv('SCRATCH'))
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

start.x<-0.5
start.y<-0.9

y.scale<-60 # page height equivalent to 100C

y.from.month<-function(year,month) {
  return((cet[year-1658,month+1]-cet.monthly.means[month])/y.scale)
}

# Precalculate the monthly offsets
max.offset<-(opt$year-1659)*12+opt$month
offsets.x<-rep(NA,max.offset)
offsets.y<-rep(NA,max.offset)
x.npc<-start.x
y.npc<-start.y
for(i in seq(1,max.offset)) {
   if(y.npc<0.05) next
   x.scale<-1/cos((y.npc-0.5)*pi)
   x.npc<-x.npc-step*x.scale
   y.npc<-y.npc-step*x.scale*pitch
   offsets.x[i]<-x.npc
   offsets.y[i]<-y.npc
}

get.offsets.from.month<-function(year,month,frame) {
  months.offset<-(opt$year-year)*12+opt$month-month
  x.npc<-offsets.x[months.offset+1]
  y.npc<-offsets.y[months.offset+1]
  if(is.na(x.npc)) return(list(x=0,y=0))
  x.scale<-1/cos((y.npc-0.5)*pi)
  if(y.npc<0.05) return(list(x=0,y=0))
  x.npc<-x.npc-step*x.scale*frame/frames.per.month
  while(x.npc<0) x.npc<-x.npc+1
  y.npc<-y.npc-step*pitch*x.scale*frame/frames.per.month
  return(list(x=x.npc,y=y.npc))
}

# Call this function for each month from Jan 1659 to end date
plot.pair<-function(year,month) {

   months.offset<-(opt$year-year)*12+opt$month-month

   offs<-get.offsets.from.month(year,month,opt$frame)
   x.npc<-offs$x
   y.npc<-offs$y
   if(y.npc<0.05) return() # Off bottom of page
   x.scale<-1/cos((y.npc-0.5)*pi)
  
   # Show normal line
   gp.grey<-gpar(col=rgb(0.5,0.5,0.5,1),fill=rgb(0.5,0.5,0.5,1),lwd=1)
   grid.lines(x=unit(c(x.npc,x.npc+step*x.scale),'npc'),
              y=unit(c(y.npc,y.npc+pitch*step*x.scale),'npc'),
              gp=gp.grey)
   if(x.npc+step>1) {
       grid.lines(x=unit(c(x.npc-1,x.npc-1+step*x.scale),'npc'),
                  y=unit(c(y.npc,y.npc+pitch*step*x.scale),'npc'),
                  gp=gp.grey)
   }

   # Show data
   gp<-gpar(col=rgb(0,0,0,1),fill=rgb(0,0,0,1),lwd=2)
   next.month<-ymd(sprintf("%04d-%02d-15",year,month))+months(1)
   grid.lines(x=unit(c(x.npc,x.npc+step*x.scale),'npc'),
              y=unit(c(y.npc+y.from.month(year,month),
                       y.npc+pitch*step*x.scale+y.from.month(year(next.month),month(next.month))),
                       'npc'),
              gp=gp)
   if(x.npc+step>1) {
      grid.lines(x=unit(c(x.npc-1,x.npc-1+step*x.scale),'npc'),
                 y=unit(c(y.npc+y.from.month(year,month),
                       y.npc+pitch*step*x.scale+y.from.month(year(next.month),month(next.month))),
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
   if(x.npc>0.995) {
       grid.points(x=unit(c(x.npc-1),'npc'),
                   y=unit(c(y.npc+y.from.month(year,month)),'npc'),
                   size=unit(0.005,'npc'),
                   pch=21,
                   gp=gp)
   }  
   if(x.npc<0.005) {
       grid.points(x=unit(c(x.npc+1),'npc'),
                   y=unit(c(y.npc+y.from.month(year,month)),'npc'),
                   size=unit(0.005,'npc'),
                   pch=21,
                   gp=gp)
   }  
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
