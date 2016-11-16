#!/usr/bin/Rscript --no-save

# Just make the streamlines for later rendering.

library(GSDF.ERA5)
library(GSDF.WeatherMap)

year<-2016
month<-1
day<-30
hour<-12

Imagedir<-'.'
if(!file.exists(Imagedir)) dir.create(Imagedir,recursive=TRUE)

Options<-WeatherMap.set.option(NULL)
Options<-WeatherMap.set.option(Options,'pole.lon',160)
Options<-WeatherMap.set.option(Options,'pole.lat',45)

Options<-WeatherMap.set.option(Options,'lat.min',-90)
Options<-WeatherMap.set.option(Options,'lat.max',90)
Options<-WeatherMap.set.option(Options,'lon.min',-190+50)
Options<-WeatherMap.set.option(Options,'lon.max',190+50)
Options$vp.lon.min<- -180+50
Options$vp.lon.max<-  180+50
Options<-WeatherMap.set.option(Options,'wrap.spherical',F)

Options<-WeatherMap.set.option(Options,'wind.vector.points',3)
Options<-WeatherMap.set.option(Options,'wind.vector.scale',0.2)
Options<-WeatherMap.set.option(Options,'wind.vector.move.scale',1)
Options<-WeatherMap.set.option(Options,'wind.vector.density',0.5)
Options$ice.points<-100000


    s<-NULL
    sf.name<-sprintf("%s/streamlines.%04d-%02d-%02d:%02d.rd",
                           Imagedir,year,month,day,hour)

    uwnd<-ERA5.get.slice.at.hour('uwnd.10m',year,month,day,hour)
    vwnd<-ERA5.get.slice.at.hour('vwnd.10m',year,month,day,hour)
    t.actual<-ERA5.get.slice.at.hour('air.2m',year,month,day,hour)
    t.normal<-readRDS(sprintf("%s/ERA5/oper/climtologies.test/air.2m.%02d.Rdata",
                           Sys.getenv('SCRATCH'),hour))
    #t.normal<-t.actual
    #t.normal$data[]<-rep(286,length(t.normal$data))
    s<-WeatherMap.make.streamlines(s,uwnd,vwnd,t.actual,t.normal,Options)
    s<-WeatherMap.make.streamlines(s,uwnd,vwnd,t.actual,t.normal,Options)
    s<-WeatherMap.make.streamlines(s,uwnd,vwnd,t.actual,t.normal,Options)
    s$status<-s$status*0+5
    save(year,month,day,hour,s,file=sf.name)

