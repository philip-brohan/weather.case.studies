#!/usr/bin/Rscript --no-save

# Global obs coverage - icoads3.0 and ICOADS2.5
# Puffersphere version.

library(GSDF)
library(GSDF.WeatherMap)
library(parallel)
library(lubridate)
library(IMMA)

year<-1800
month<-1
day<-2
hour<-0
n.total<-as.integer(365.25*(150)-4) # Total number of days to be rendered

Imagedir<-sprintf("%s/images/icoads_3.0",Sys.getenv('SCRATCH'))
if(!file.exists(Imagedir)) dir.create(Imagedir,recursive=TRUE)
datadir<-'/data/local/hadpb'

use.cores<-7

c.date<-ymd_hms(sprintf("%04d/%02d/%02d %02d:00:00",year,month,day,hour))

Options<-WeatherMap.set.option(NULL)
Options<-WeatherMap.set.option(Options,'lat.min',-90)
Options<-WeatherMap.set.option(Options,'lat.max',90)
Options<-WeatherMap.set.option(Options,'lon.min',-190)
Options<-WeatherMap.set.option(Options,'lon.max',190)
Options$vp.lon.min<- -180
Options$vp.lon.max<-  180
Options<-WeatherMap.set.option(Options,'wrap.spherical',T)
Options<-WeatherMap.set.option(Options,'show.mslp',F)
Options<-WeatherMap.set.option(Options,'show.ice',T)
Options<-WeatherMap.set.option(Options,'show.obs',T)
Options<-WeatherMap.set.option(Options,'show.fog',F)
Options<-WeatherMap.set.option(Options,'show.wind',F)
Options<-WeatherMap.set.option(Options,'show.temperature',F)
Options<-WeatherMap.set.option(Options,'show.precipitation',F)
Options<-WeatherMap.set.option(Options,'temperature.range',12)
Options<-WeatherMap.set.option(Options,'obs.size',1.5)
Options<-WeatherMap.set.option(Options,'obs.colour',rgb(255,0,0,255,
                                                       maxColorValue=255))
Options<-WeatherMap.set.option(Options,'land.colour',rgb(0,0,0,255,
                                                       maxColorValue=255))
Options<-WeatherMap.set.option(Options,'sea.colour',rgb(100,100,100,255,
                                                       maxColorValue=255))
Options<-WeatherMap.set.option(Options,'pole.lon',160)
Options<-WeatherMap.set.option(Options,'pole.lat',35)
Options<-WeatherMap.set.option(Options,'background.resolution','high')
Options<-WeatherMap.set.option(Options,'ice.colour',Options$land.colour)
                               

obs.cache<-list()
ReadObs.cache<-function(file.name,start,end) {
  result<-NULL
  if(!is.null(obs.cache[[file.name]])) {
    result<-obs.cache[[file.name]]
  } else {
     if(length(names(obs.cache))>2) {
       obs.cache<-list()
       gc(verbose=FALSE)
     }
     obs.cache[[file.name]]<-ReadObs(file.name)
    result<-obs.cache[[file.name]]
  }
  w<-which(is.na(result$HR))
  if(length(w)>0) result$HR[w]<-12
  result.dates<-ymd_hms(sprintf("%04d-%02d-%02d %02d:%02d:00",
                                as.integer(result$YR),
                                as.integer(result$MO),
                                as.integer(result$DY),
                                as.integer(result$HR),
                                as.integer((result$HR%%1)*60)))
  w<-which(result.dates>=start & result.dates<end)
  result<-result[w,]  
  return(result)
}
                                        # Get observations from ICOADS
ICOADS.3.0.get.obs<-function(year,month,day,hour,duration) {
  start<-ymd_hms(sprintf("%04d-%02d-%02d %02d:30:00",year,month,day,hour))-
    hours(duration/2)
  end<-start+hours(duration)
  files<-unique(c(sprintf("%s/icoads_3.0/ICOADS_R3_Beta3_%04d%02d.dat.gz",
                        datadir,as.integer(year(start)),
                                as.integer(month(start))),
                  sprintf("%s/icoads_3.0/ICOADS_R3_Beta3_%04d%02d.dat.gz",
                        datadir,as.integer(year(end)),
                                as.integer(month(end)))))
  result<-data.frame()
  for(file in files) {
    o<-ReadObs.cache(file,start,end)
    if(length(colnames(result))==0) {
      result<-o
    } else {
      cols <- intersect(colnames(result), colnames(o))
      result<-rbind(result[,cols], o[,cols])
    }
  }
  w<-which(result$LON>180)
  if(length(w)>0) result$LON[w]<- result$LON[w]-360
  return(result)
}

land<-WeatherMap.get.land(Options)

plot.day<-function(l.count) {    

    n.date<-c.date+days(l.count)
    year<-year(n.date)
    month<-month(n.date)
    day<-day(n.date)
    #hour<-hours(n.date)
    hour<-12

    image.name<-sprintf("%04d-%02d-%02d:%02d.png",year,month,day,hour)

    ifile.name<-sprintf("%s/%s",Imagedir,image.name)
    if(file.exists(ifile.name) && file.info(ifile.name)$size>0) return()
    print(sprintf("%d %04d-%02d-%02d - %s",l.count,year,month,day,
                   Sys.time()))

    obs.ic<-ICOADS.3.0.get.obs(year,month,day,hour,72)

     png(ifile.name,
             width=1080*WeatherMap.aspect(Options),
             height=1080,
             bg=Options$sea.colour,
             pointsize=24,
             type='cairo')
    Options$label<-sprintf("%04d-%02d-%02d",year,month,day)
          pushViewport(dataViewport(c(Options$vp.lon.min,Options$vp.lon.max),
                                    c(Options$lat.min,Options$lat.max),
                                    extension=0))
          WeatherMap.draw.land(NULL,Options)
          if(length(obs.ic$LAT)>0) {
             obs.ic$Latitude<-obs.ic$LAT
             obs.ic$Longitude<-obs.ic$LON
             WeatherMap.draw.obs(obs.ic,Options)
           }
          upViewport()
    dev.off()
    gc(verbose=FALSE)
}

mclapply(seq(0,n.total),plot.day,mc.cores=use.cores,mc.preschedule=TRUE)
#lapply(seq(0,n.total),plot.day)
