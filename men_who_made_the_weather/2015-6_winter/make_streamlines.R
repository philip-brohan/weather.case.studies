#!/usr/bin/Rscript --no-save

# Just make the streamlines for later rendering.

library(GSDF.ERAI)
library(GSDF.WeatherMap)
library(chron)

Year<-2015
Month<-11
Day<-1
Hour<-12
n.total<-131*24

c.date<-chron(dates=sprintf("%04d/%02d/%02d",Year,Month,Day),
          times=sprintf("%02d:00:00",Hour),
          format=c(dates='y/m/d',times='h:m:s'))


GSDF.cache.dir<-sprintf("%s/GSDF.cache",Sys.getenv('SCRATCH'))
if(!file.exists(GSDF.cache.dir)) dir.create(GSDF.cache.dir,recursive=TRUE)
Imagedir<-sprintf("%s/images/UK_winter_2015",Sys.getenv('SCRATCH'))
if(!file.exists(Imagedir)) dir.create(Imagedir,recursive=TRUE)

Options<-WeatherMap.set.option(NULL)
Options<-WeatherMap.set.option(Options,'pole.lon',160)
Options<-WeatherMap.set.option(Options,'pole.lat',36)

range<-20
aspect<-16/9
Options<-WeatherMap.set.option(Options,'lat.min',range*-1-5)
Options<-WeatherMap.set.option(Options,'lat.max',range+5)
Options<-WeatherMap.set.option(Options,'lon.min',range*aspect*-1-5)
Options<-WeatherMap.set.option(Options,'lon.max',range*aspect+5)
Options<-WeatherMap.set.option(Options,'wrap.spherical',F)

Options<-WeatherMap.set.option(Options,'wind.vector.points',3)
Options<-WeatherMap.set.option(Options,'wind.vector.scale',0.5)
Options<-WeatherMap.set.option(Options,'wind.vector.move.scale',10)
Options<-WeatherMap.set.option(Options,'wind.vector.density',1.5)
Options$ice.points<-100000


make.streamlines<-function(year,month,day,hour,Options,streamlines=NULL) {

    sf.name<-sprintf("%s/streamlines.%04d-%02d-%02d:%02d.rd",
                           Imagedir,year,month,day,hour)
    if(file.exists(sf.name) && file.info(sf.name)$size>5000) {
       load(sf.name)
       return(s)
    }
    print(sprintf("%04d-%02d-%02d:%02d - %s",year,month,day,hour,
                   Sys.time()))

    uwnd<-ERAI.get.slice.at.hour('uwnd.10m',year,month,day,hour)
    vwnd<-ERAI.get.slice.at.hour('vwnd.10m',year,month,day,hour)
    t.actual<-ERAI.get.slice.at.hour('air.2m',year,month,day,hour)
    t.normal<-t.actual
    t.normal$data[]<-rep(286,length(t.normal$data))
    s<-WeatherMap.make.streamlines(streamlines,uwnd,vwnd,t.actual,t.normal,Options)
    if(max(s$status)<4) s$status<-s$status*0+5
    save(year,month,day,hour,s,file=sf.name)
    gc(verbose=FALSE)
    return(s)

}

s<-NULL
for(n.count in seq(1,n.total)) {

    n.date<-c.date+n.count/24
    year<-as.numeric(as.character(chron::years(n.date)))
    month<-months(n.date)
    day<-chron::days(n.date)
    hour<-(n.count+Hour)%%24

    # serial component - streamlines evolve from hour to hour
    s<-make.streamlines(year,month,day,hour,Options,streamlines=s)

}
