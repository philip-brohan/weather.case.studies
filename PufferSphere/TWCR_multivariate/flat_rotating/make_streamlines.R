#!/usr/bin/Rscript --no-save

# Just make the streamlines for later rendering.

library(GSDF.TWCR)
library(GSDF.WeatherMap)
library(grid)
library(deldir)
library(RColorBrewer)
library(chron)

Year<-2014
Month<-1
Day<-1
Hour<-1
n.total<-365*24
version<-'3.5.1'
cores<-6

c.date<-chron(dates=sprintf("%04d/%02d/%02d",Year,Month,Day),
          times=sprintf("%02d:00:00",Hour),
          format=c(dates='y/m/d',times='h:m:s'))


GSDF.cache.dir<-sprintf("%s/GSDF.cache",Sys.getenv('SCRATCH'))
if(!file.exists(GSDF.cache.dir)) dir.create(GSDF.cache.dir,recursive=TRUE)
Imagedir<-sprintf("%s/images/TWCR_multivariate_rotating",Sys.getenv('SCRATCH'))
if(!file.exists(Imagedir)) dir.create(Imagedir,recursive=TRUE)

Options<-WeatherMap.set.option(NULL)
Options<-WeatherMap.set.option(Options,'pole.lon',160)
Options<-WeatherMap.set.option(Options,'pole.lat',35)

Options<-WeatherMap.set.option(Options,'lat.min',-90)
Options<-WeatherMap.set.option(Options,'lat.max',90)
Options<-WeatherMap.set.option(Options,'lon.min',-190)
Options<-WeatherMap.set.option(Options,'lon.max',190)
Options$vp.lon.min<- -180
Options$vp.lon.max<-  180
Options<-WeatherMap.set.option(Options,'wrap.spherical',F)

Options<-WeatherMap.set.option(Options,'wind.vector.points',3)
Options<-WeatherMap.set.option(Options,'wind.vector.scale',0.1)
Options<-WeatherMap.set.option(Options,'wind.vector.move.scale',1)
Options<-WeatherMap.set.option(Options,'wind.vector.density',1)
Options$ice.points<-100000

set.pole<-function(step,Options) {
  if(step<=1000) return(Options)
  lon<-160+((step-1000)/10)
  if(lon>360) lon<-lon%%360
  lat<-35+sin((step-1000)/500)*20
  Options<-WeatherMap.set.option(Options,'pole.lon',lon)
  Options<-WeatherMap.set.option(Options,'pole.lat',lat)
  min.lon<-((step-1000)/5)%%360-180
  Options<-WeatherMap.set.option(Options,'lon.min',min.lon-10)
  Options<-WeatherMap.set.option(Options,'lon.max',min.lon+380)
  Options<-WeatherMap.set.option(Options,'vp.lon.min',min.lon   )
  Options<-WeatherMap.set.option(Options,'vp.lon.max',min.lon+360)
  return(Options)
}

make.streamlines<-function(year,month,day,hour,Options,streamlines=NULL) {

    sf.name<-sprintf("%s/streamlines.%04d-%02d-%02d:%02d.rd",
                           Imagedir,year,month,day,hour)
    if(file.exists(sf.name) && file.info(sf.name)$size>5000) {
       load(sf.name)
       return(s)
    }
    print(sprintf("%04d-%02d-%02d:%02d - %s",year,month,day,hour,
                   Sys.time()))

    uwnd<-TWCR.get.slice.at.hour('uwnd.10m',year,month,day,hour,version=version)
    vwnd<-TWCR.get.slice.at.hour('vwnd.10m',year,month,day,hour,version=version)
    t.actual<-TWCR.get.slice.at.hour('air.2m',year,month,day,hour,version=version)
    t.normal<-t.actual
    t.normal$data[]<-rep(286,length(t.normal$data))
    s<-WeatherMap.make.streamlines(streamlines,uwnd,vwnd,t.actual,t.normal,Options)
    save(year,month,day,hour,s,file=sf.name)
    gc(verbose=FALSE)
    return(s)

}

s<-NULL
for(n.count in seq(999,n.total)) {

    n.date<-c.date+n.count/24
    year<-as.numeric(as.character(years(n.date)))
    month<-months(n.date)
    day<-days(n.date)
    hour<-(n.count+Hour)%%24

    Options<-set.pole(n.count,Options)

    # serial component - streamlines evolve from hour to hour
    s<-make.streamlines(year,month,day,hour,Options,streamlines=s)

}
