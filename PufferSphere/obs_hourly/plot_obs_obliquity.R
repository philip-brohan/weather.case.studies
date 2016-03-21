#!/usr/bin/Rscript --no-save

# Show surface observations network.
# Spherical version with garish colours
# Move the north pole to centre UK - shows up better on PufferSphere?

library(GSDF.WeatherMap)
library(GSDF)
library(parallel)
library(lubridate)

Year<-2014
Month<-1
Day<-2
Hour<-0
n.total<-24*6*10
cores<-6

Imagedir<-sprintf("%s/images/ISPD_spherical_obliquity/%04d",Sys.getenv('SCRATCH'),Year)
if(!file.exists(Imagedir)) dir.create(Imagedir,recursive=TRUE)

c.date<-ymd_hms(sprintf("%04d/%02d/%02d %02d:%02d:00",
                   Year,Month,Day,as.integer(Hour), as.integer((Hour%%1)*60)))

Options<-WeatherMap.set.option(NULL)
Options<-WeatherMap.set.option(Options,'lat.min',-90)
Options<-WeatherMap.set.option(Options,'lat.max',90)
Options<-WeatherMap.set.option(Options,'lon.min',-190)
Options<-WeatherMap.set.option(Options,'lon.max',190)
Options$vp.lon.min<- -180
Options$vp.lon.max<-  180
Options<-WeatherMap.set.option(Options,'wrap.spherical',T)
Options<-WeatherMap.set.option(Options,'obs.size',1.5)
Options<-WeatherMap.set.option(Options,'obs.colour',rgb(255,215,0,255,
                                                       maxColorValue=255))
Options<-WeatherMap.set.option(Options,'land.colour',rgb(0,0,0,255,
                                                       maxColorValue=255))
Options<-WeatherMap.set.option(Options,'sea.colour',rgb(100,100,100,255,
                                                       maxColorValue=255))
Options<-WeatherMap.set.option(Options,'pole.lon',160)
Options<-WeatherMap.set.option(Options,'pole.lat',35)
Options<-WeatherMap.set.option(Options,'background.resolution','high')

aspect<-2

land<-WeatherMap.get.land(Options)

hourly.obs.dir<-'/data/users/hadpb/20CR/version_3.5.1/observations.by.hour'
load.hourly.obs<-function(year,month,day,hour,spread=1) {
  nearest.file<-sprintf("%s/%04d%02d%02d%02d.Rdata",hourly.obs.dir,
                                         year,month,day,as.integer(hour))
  if(as.integer(hour)==0) {
    dte<-ymd(sprintf("%04d%02d%02d",year,month,day))-days(1)
    previous.file<-sprintf("%s/%04d%02d%02d%02d.Rdata",hourly.obs.dir,year(dte),
                           month(dte),day(dte),23)
  } else {
   previous.file<-sprintf("%s/%04d%02d%02d%02d.Rdata",hourly.obs.dir,
                                year,month,day,as.integer(hour)-1)
 }
  if(as.integer(hour)==23) {
    dte<-ymd(sprintf("%04d%02d%02d",year,month,day))+days(1)
    next.file<-sprintf("%s/%04d%02d%02d%02d.Rdata",hourly.obs.dir,year(dte),
                           month(dte),day(dte),0)
  } else {
   next.file<-sprintf("%s/%04d%02d%02d%02d.Rdata",hourly.obs.dir,
                                year,month,day,as.integer(hour)+1)
 }
  if(!file.exists(nearest.file)) stop(sprintf("No obs file %s",nearest.file))
  load(nearest.file)
  obs.hr$Time.Offset<-obs.hr$Time.Offset-min(obs.hr$Time.Offset,na.rm=TRUE)
  result<-obs.hr
  if(file.exists(previous.file)) {
    load(previous.file)
    obs.hr$Time.Offset<-obs.hr$Time.Offset-min(obs.hr$Time.Offset,na.rm=TRUE)-1
    result<-rbind(result,obs.hr)
  }
   if(file.exists(next.file)) {
    load(previous.file)
    obs.hr$Time.Offset<-obs.hr$Time.Offset-min(obs.hr$Time.Offset,na.rm=TRUE)+1
    result<-rbind(result,obs.hr)
  }
  result$Time.Offset<-result$Time.Offset-hour%%1
  w<-which(abs(result$Time.Offset)<=spread)
  result<-result[w,]
  w<-which(result$Longitude>180)
  if(length(w)>0) result$Longitude[w]<-result$Longitude[w]-360
  return(result)
}

plot.hour<-function(l.count) {

    n.date<-c.date+minutes(l.count*6)
    year<-year(n.date)
    month<-month(n.date)
    day<-day(n.date)
    hour<-hour(n.date)+minute(n.date)/60
    image.name<-sprintf("%04d-%02d-%02d:%02d:%02d.png",year,month,day,as.integer(hour),
                                 as.integer((hour%%1)*60))

    ifile.name<-sprintf("%s/%s",Imagedir,image.name)
    if(file.exists(ifile.name) && file.info(ifile.name)$size>0) return()

    obs<-load.hourly.obs(year,month,day,hour)
    
     png(ifile.name,
             width=1050*WeatherMap.aspect(Options),
             height=1050,
             bg=Options$sea.colour,
             pointsize=24,
             type='cairo')
          pushViewport(dataViewport(c(Options$vp.lon.min,Options$vp.lon.max),
                                    c(Options$lat.min,Options$lat.max),
                                    extension=0))
          WeatherMap.draw.land(NULL,Options)
          Options<-WeatherMap.set.option(Options,'obs.colour',
                                       rgb(255,215,0,255,maxColorValue=255))
          for(size.class in seq(3,10)) {
            w<-which(abs(obs$Time.Offset)<size.class/10)
            ob.size<-1.5/(1+exp(size.class-7))
            Options<-WeatherMap.set.option(Options,'obs.size',ob.size)
            WeatherMap.draw.obs(obs[w,],Options)
            obs<-obs[-w,]
          }
         upViewport()
    dev.off()
    gc(verbose=FALSE)
}

r<-mclapply(seq(0,n.total),plot.hour,mc.cores=cores,mc.preschedule=TRUE)

