#!/usr/bin/Rscript --no-save

# ICOADS 3.0 stationary, standard projection

library(GSDF)
library(GSDF.WeatherMap)
library(getopt)
library(lubridate)
library(RColorBrewer)
library(IMMA)

opt = getopt(matrix(c(
  'year',   'd', 2, "integer",
  'month',  'm', 2, "integer",
  'day',    'e', 2, "integer"
),ncol=4,byrow=TRUE))
if ( is.null(opt$year) )   { stop("Year not specified") }
if ( is.null(opt$month) )  { stop("Month not specified") }
if ( is.null(opt$day) )    { stop("Day not specified") }

Imagedir<-sprintf("%s/images/ICOADS3.boring",Sys.getenv('SCRATCH'))
if(!file.exists(Imagedir)) dir.create(Imagedir,recursive=TRUE)

Options<-WeatherMap.set.option(NULL)
Options<-WeatherMap.set.option(Options,'land.colour',rgb(0,0,0,255,
                                                       maxColorValue=255))
Options<-WeatherMap.set.option(Options,'sea.colour',rgb(100,100,100,255,
                                                       maxColorValue=255))
Options<-WeatherMap.set.option(Options,'ice.colour',Options$land.colour)
Options<-WeatherMap.set.option(Options,'background.resolution','high')

Options<-WeatherMap.set.option(Options,'lat.min',-90)
Options<-WeatherMap.set.option(Options,'lat.max',90)
Options<-WeatherMap.set.option(Options,'lon.min',-180)
Options<-WeatherMap.set.option(Options,'lon.max',180)
Options$vp.lon.min<- -180
Options$vp.lon.max<-  180
Options$obs.size<- 1.0

land<-WeatherMap.get.land(Options)
land<-GSDF:::GSDF.pad.longitude(land)

ReadObs.cache<-function(file.name,start,end) {
  result<-data.frame()
  batch.length<-100000
  f.in<-file(file.name)
  open(f.in)
  while(batch.length==100000) {
      result.batch<-ReadObs(f.in,100000)
      batch.length<-length(result.batch$HR)
      w<-which(is.na(result.batch$HR))
      if(length(w)>0) result.batch$HR[w]<-12
      result.dates<-ymd_hms(sprintf("%04d-%02d-%02d %02d:%02d:00",
                                    as.integer(result.batch$YR),
                                    as.integer(result.batch$MO),
                                    as.integer(result.batch$DY),
                                    as.integer(result.batch$HR),
                                    as.integer((result.batch$HR%%1)*60)))
      w<-which(result.dates>=start & result.dates<end)
      if(length(w)==0) next
      result.batch<-result.batch[w,]
        if(length(colnames(result))==0) {
          result<-result.batch
        } else {
          cols <- intersect(colnames(result), colnames(result.batch))
          result<-rbind(result[,cols], result.batch[,cols])
        }
     gc(verbose=FALSE)
  }
  return(result)
}

ICOADS.3.0.get.obs<-function(year,month,day,hour,duration) {
  start<-ymd_hms(sprintf("%04d-%02d-%02d %02d:30:00",year,month,day,hour))-
    hours(duration/2)
  end<-start+hours(duration)
  files<-unique(c(sprintf("%s/ICOADS3/IMMA1_R3.0.0_%04d-%02d.gz",
                        Sys.getenv('SCRATCH'),as.integer(year(start)),
                                as.integer(month(start))),
                  sprintf("%s/ICOADS3/IMMA1_R3.0.0_%04d-%02d.gz",
                        Sys.getenv('SCRATCH'),as.integer(year(end)),
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

plot.day<-function(year,month,day) {    

    image.name<-sprintf("%04d-%02d-%02d.png",year,month,day)
    ifile.name<-sprintf("%s/%s",Imagedir,image.name)
    if(file.exists(ifile.name) && file.info(ifile.name)$size>0) return()

    obs.ic<-ICOADS.3.0.get.obs(year,month,day,12,72)
    
     png(ifile.name,
             width=1080*16/9,
             height=1080,
             bg=Options$sea.colour,
             pointsize=24,
             type='cairo-png')
    Options$label<-sprintf("%04d-%02d-%02d",year,month,day)
  
  	   pushViewport(dataViewport(c(Options$vp.lon.min,Options$vp.lon.max),
  				     c(Options$lat.min,Options$lat.max),
  				      extension=0))
      WeatherMap.draw.land(land,Options)
          if(length(obs.ic$LAT)>0) {
             obs.ic$Latitude<-obs.ic$LAT
             obs.ic$Longitude<-obs.ic$LON
             WeatherMap.draw.obs(obs.ic,Options)
           }
      Options<-WeatherMap.set.option(Options,'land.colour',rgb(100,100,100,255,
                                                           maxColorValue=255))
      WeatherMap.draw.label(Options)
    dev.off()
  }


plot.day(opt$year,opt$month,opt$day)

