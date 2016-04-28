#!/usr/bin/Rscript --no-save

# Show weather and fog in Gil's scout reanalysis.
# Spherical version with garish colours
# Single field to test bridson with no scaling

library(GSDF.TWCR)
library(GSDF.WeatherMap)
library(chron)
library(png)

Year<-2014
Month<-1
Day<-1
Hour<-0
n.total<-0
version<-'3.5.1'
cores<-6

fog.threshold<-exp(1)

GSDF.cache.dir<-sprintf("%s/GSDF.cache",Sys.getenv('SCRATCH'))
if(!file.exists(GSDF.cache.dir)) dir.create(GSDF.cache.dir,recursive=TRUE)
Imagedir<-sprintf("%s/images/bridson_noscale",Sys.getenv('SCRATCH'))
if(!file.exists(Imagedir)) dir.create(Imagedir,recursive=TRUE)

c.date<-chron(dates=sprintf("%04d/%02d/%02d",Year,Month,Day),
          times=sprintf("%02d:00:00",Hour),
          format=c(dates='y/m/d',times='h:m:s'))

Options<-WeatherMap.set.option(NULL)
Options<-WeatherMap.set.option(Options,'lat.min',-90)
Options<-WeatherMap.set.option(Options,'lat.max',90)
Options<-WeatherMap.set.option(Options,'lon.min',-190)
Options<-WeatherMap.set.option(Options,'lon.max',190)
Options$vp.lon.min<- -180
Options$vp.lon.max<-  180
Options<-WeatherMap.set.option(Options,'jitter',FALSE)
Options<-WeatherMap.set.option(Options,'wrap.spherical',F)
Options<-WeatherMap.set.option(Options,'show.mslp',F)
Options<-WeatherMap.set.option(Options,'show.ice',F)
Options<-WeatherMap.set.option(Options,'show.obs',F)
Options<-WeatherMap.set.option(Options,'show.fog',F)
Options<-WeatherMap.set.option(Options,'show.precipitation',F)
Options<-WeatherMap.set.option(Options,'temperature.range',25)
Options<-WeatherMap.set.option(Options,'wind.palette',
                       diverge_hcl(7,c=c(150,0),l=c(25,30),power=1))
#Options<-WeatherMap.set.option(Options,'pole.lon',160)
#Options<-WeatherMap.set.option(Options,'pole.lat',35)
Options<-WeatherMap.set.option(Options,'background.resolution','high')

aspect<-2

Options$ice.points<-100000
Options$wind.vector.lwd<-4
Options$wind.vector.move.scale<-Options$wind.vector.move.scale/3
Options$wind.vector.density<-Options$wind.vector.density*1
Options$wind.vector.scale<-Options$wind.vector.scale*1
land<-WeatherMap.get.land(Options)

Options<-WeatherMap.set.option(Options,'cores',4)

make.streamlines<-function(year,month,day,hour,streamlines=NULL) {


    sf.name<-sprintf("%s/streamlines.%04d-%02d-%02d:%02d.rd",
                           Imagedir,year,month,day,hour)
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

plot.hour<-function(year,month,day,hour,streamlines) {


    image.name<-sprintf("%04d-%02d-%02d:%02d.png",year,month,day,hour)

    ifile.name<-sprintf("%s/%s",Imagedir,image.name)

     png(ifile.name,
             width=1050*WeatherMap.aspect(Options),
             height=1050,
             bg=Options$sea.colour,
             pointsize=24,
             type='cairo')
    Options$label<-sprintf("")
       WeatherMap.draw(Options=Options,uwnd=NULL,icec=NULL,
                          vwnd=NULL,precip=NULL,mslp=NULL,
                          t.actual=NULL,t.normal=NULL,land=land,
                          fog=NULL,obs=NULL,streamlines=streamlines)
    dev.off()
}


n.count<-0
s<-NULL
    n.date<-c.date+n.count/24
    year<-as.numeric(as.character(years(n.date)))
    month<-months(n.date)
    day<-days(n.date)
    hour<-(n.count+Hour)%%24

    # serial component - streamlines evolve from hour to hour
    s<-make.streamlines(year,month,day,hour,streamlines=s)
    # set the status to make all the lines visible
    s$status<-s$status*0+4

    image.name<-sprintf("%04d-%02d-%02d:%02d.png",year,month,day,hour)
    ifile.name<-sprintf("%s/%s",Imagedir,image.name)

    plot.hour(year,month,day,hour,s)
 
   