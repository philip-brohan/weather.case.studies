#!/usr/common/graphics/R/R-3.1.0/bin/R --no-save

# Overland expedition - Bear in 1897-8

library(GSDF.TWCR)
library(GSDF.WeatherMap)
library(parallel)

Year<-1897
Month<-6
Day<-2
Hour<-0
n.total<-365*24 # Total number of hours to be rendered
version<-'3.5.4'
fog.threshold<-exp(1)

GSDF.cache.dir<-sprintf("%s/GSDF.cache",Sys.getenv('SCRATCH'))
if(!file.exists(GSDF.cache.dir)) dir.create(GSDF.cache.dir,recursive=TRUE)
Imagedir<-sprintf("%s/images/oW3.streamlines.%s",Sys.getenv('SCRATCH'),version)
if(!file.exists(Imagedir)) dir.create(Imagedir,recursive=TRUE)

c.date<-chron(dates=sprintf("%04d/%02d/%02d",Year,Month,Day),
          times=sprintf("%02d:00:00",Hour),
          format=c(dates='y/m/d',times='h:m:s'))

Options<-WeatherMap.set.option(NULL)
Options<-WeatherMap.set.option(Options,'show.mslp',T)
Options<-WeatherMap.set.option(Options,'show.ice',T)
Options<-WeatherMap.set.option(Options,'show.obs',T)
Options<-WeatherMap.set.option(Options,'show.fog',T)
Options<-WeatherMap.set.option(Options,'show.precipitation',T)
Options<-WeatherMap.set.option(Options,'temperature.range',12)
Options<-WeatherMap.set.option(Options,'obs.size',0.5)
Options<-WeatherMap.set.option(Options,'obs.colour',rgb(255,215,0,255,
                                                       maxColorValue=255))
Options<-WeatherMap.set.option(Options,'lat.min',-40)
Options<-WeatherMap.set.option(Options,'lat.max',40)
Options<-WeatherMap.set.option(Options,'lon.min',-70)
Options<-WeatherMap.set.option(Options,'lon.max',70)
Options<-WeatherMap.set.option(Options,'pole.lon',80)
Options<-WeatherMap.set.option(Options,'pole.lat',20)
Options$label.yp<-0.03

Options$ice.points<-50000
land<-WeatherMap.get.land(Options)

Options$ice.points<-50000
Options$wind.vector.lwd<-4
land<-WeatherMap.get.land(Options)

# Rotate a set of streamlines to a new pole
rotate.streamlines<-function(s,pole.lat,pole.lon) {
   if(is.null(s)) return(NULL)
   if(!is.null(s$pole.lat)) {
     nl<-GSDF.rg.to.ll(s$y,s$x,s$pole.lat,s$pole.lon)
     s$x[]<-nl$lon
     s$y[]<-nl$lat
   }
   nl<-GSDF.ll.to.rg(s$y,s$x,pole.lat,pole.lon)
   s$x[]<-nl$lon
   s$y[]<-nl$lat
   s$pole.lat<-pole.lat
   s$pole.lon<-pole.lon
   return(s)
}

make.streamlines<-function(year,month,day,hour,streamlines=NULL) {


    sf.name<-sprintf("%s/streamlines.%04d-%02d-%02d:%02d.rd",
                           Imagedir,year,month,day,hour)
    if(file.exists(sf.name) && file.info(sf.name)$size>500000) {
       load(sf.name)
       return(s)
    }
    print(sprintf("%04d-%02d-%02d:%02d - %s",year,month,day,hour,
                   Sys.time()))

    uwnd<-TWCR.get.slice.at.hour('uwnd.10m',year,month,day,hour,version=version)
    vwnd<-TWCR.get.slice.at.hour('vwnd.10m',year,month,day,hour,version=version)
    t.actual<-TWCR.get.slice.at.hour('air.2m',year,month,day,hour,version=version)
    t.normal<-t.actual
    t.normal$data[]<-rep(283,length(t.normal$data))
    s<-WeatherMap.make.streamlines(streamlines,uwnd,vwnd,t.actual,t.normal,Options)
    save(year,month,day,hour,s,file=sf.name)
    gc(verbose=FALSE)
    return(s)

}

plot.hour<-function(year,month,day,hour,streamlines) {    

    image.name<-sprintf("%04d-%02d-%02d:%02d.png",year,month,day,hour)

    ifile.name<-sprintf("%s/%s",Imagedir,image.name)
    if(file.exists(ifile.name) && file.info(ifile.name)$size>0) return()

    prmsl<-TWCR.get.slice.at.hour('prmsl',year,month,day,hour,version=version)
    prmsl.spread<-TWCR.get.slice.at.hour('prmsl',year,month,day,hour,version=version,
                                              type='spread')
    prmsl.sd<-TWCR.get.slice.at.hour('prmsl',year,month,day,hour,
                                         version='3.4.1',type='standard.deviation')
    prmsl.normal<-TWCR.get.slice.at.hour('prmsl',year,month,day,hour,version='3.4.1',
                                             type='normal')
    fog<-TWCR.relative.entropy(prmsl.normal,prmsl.sd,prmsl,prmsl.spread)
    fog$data[]<-1-pmin(fog.threshold,pmax(0,fog$data))/fog.threshold
    prate<-NULL
    if(Options$show.precipitation) {
       prate<-TWCR.get.slice.at.hour('prate',year,month,day,hour,version=version)
     }
    icec<-TWCR.get.slice.at.hour('icec',year,month,day,hour,version=version)
    obs<-TWCR.get.obs(year,month,day,hour,version=version)
    w<-which(obs$Longitude>180)
    obs$Longitude[w]<-obs$Longitude[w]-360

     png(ifile.name,
             width=1080*WeatherMap.aspect(Options),
             height=1080,
             bg=Options$sea.colour,
             pointsize=24,
             type='cairo')
    Options$label<-sprintf("%04d-%02d-%02d:%02d",year,month,day,hour)
       WeatherMap.draw(Options=Options,uwnd=NULL,icec=icec,
                          vwnd=NULL,precip=prate,mslp=prmsl,
                          t.actual=NULL,t.normal=NULL,land=land,
                          fog=fog,obs=obs,streamlines=streamlines)
    pushViewport(dataViewport(c(Options$lon.min,Options$lon.max),
                              c(Options$lat.min,Options$lat.max),
                                extension=0))
    Options<-WeatherMap.set.option(Options,'obs.size',0.75)
    Options<-WeatherMap.set.option(Options,'obs.colour',rgb(255,0,0,255,
						   maxColorValue=255))
    w<-grep('Bear',obs$ID)
    if(length(w)>0) WeatherMap.draw.obs(obs[w,],Options)
    Options<-WeatherMap.set.option(Options,'obs.size',0.5)
    Options<-WeatherMap.set.option(Options,'obs.colour',rgb(255,215,0,255,
						   maxColorValue=255))
    upViewport()

    dev.off()
}

s<-NULL
for(n.count in seq(0,n.total)) {

    n.date<-c.date+n.count/24
    year<-as.numeric(as.character(years(n.date)))
    month<-months(n.date)
    day<-days(n.date)
    hour<-n.count%%24

    # serial component - streamlines evolve from hour to hour
    s<-make.streamlines(year,month,day,hour,streamlines=s)

    image.name<-sprintf("%04d-%02d-%02d:%02d.png",year,month,day,hour)
    ifile.name<-sprintf("%s/%s",Imagedir,image.name)
    if(file.exists(ifile.name) && file.info(ifile.name)$size>0) next
    # Each plot in a seperate parallel process
    mcparallel(plot.hour(year,month,day,hour,s))
    if(hour==23) mccollect(wait=TRUE)

}
mccollect()
