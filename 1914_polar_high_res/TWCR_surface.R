#!/usr/bin/Rscript --no-save

# Arctic 1915 - high-res plot

library(GSDF.TWCR)
library(GSDF.WeatherMap)
library(parallel)

year<-1915
month<-1
day<-26
hour<-12

GSDF.cache.dir<-sprintf("%s/GSDF.cache",Sys.getenv('SCRATCH'))
if(!file.exists(GSDF.cache.dir)) dir.create(GSDF.cache.dir,recursive=TRUE)
Imagedir<-sprintf("%s/images/Arctic.1915.high.res",Sys.getenv('SCRATCH'))
if(!file.exists(Imagedir)) dir.create(Imagedir,recursive=TRUE)

c.date<-chron(dates=sprintf("%04d/%02d/%02d",year,month,day),
          times=sprintf("%02d:00:00",hour),
          format=c(dates='y/m/d',times='h:m:s'))

Options<-WeatherMap.set.option(NULL)
Options<-WeatherMap.set.option(Options,'show.mslp',T)
Options<-WeatherMap.set.option(Options,'show.ice',T)
Options<-WeatherMap.set.option(Options,'show.obs',T)
Options<-WeatherMap.set.option(Options,'show.fog',T)
Options<-WeatherMap.set.option(Options,'show.precipitation',T)
Options<-WeatherMap.set.option(Options,'temperature.range',12)
Options<-WeatherMap.set.option(Options,'obs.size',0.5)

range<-60
aspect<-4/3
Options<-WeatherMap.set.option(Options,'lat.min',range*-1)
Options<-WeatherMap.set.option(Options,'lat.max',range)
Options<-WeatherMap.set.option(Options,'lon.min',range*aspect*-1)
Options<-WeatherMap.set.option(Options,'lon.max',range*aspect)
Options<-WeatherMap.set.option(Options,'pole.lon',115)
Options<-WeatherMap.set.option(Options,'pole.lat',1)

Options$wind.vector.seed<-Options$wind.vector.seed*0.45*1.5
Options$ice.points<-50000
Options$wind.palette.opacity<-1.0
Options$wind.vector.scale<-0.2
land<-WeatherMap.get.land(Options)

Options$mslp.lwd<-2
Options$mslp.step=350                       # Smaller -more contours
Options$mslp.tpscale=3500                    # Smaller -contours less transparent

get.member.at.hour<-function(variable,year,month,day,hour,member,version='3.5.1') {

       t<-TWCR.get.members.slice.at.hour(variable,year,month,day,
                                  hour,version=version)
       t<-GSDF.select.from.1d(t,'ensemble',member)
       gc()
       return(t)
  }

make.streamlines<-function(year,month,day,hour,streamlines=NULL) {

   # sf.name<-sprintf("%s/streamlines.%04d-%02d-%02d:%02d.rd",
   #                        Imagedir,year,month,day,hour)
   # if(file.exists(sf.name) && file.info(sf.name)$size>0) {
   #    load(sf.name)
   #    return(s)
   # }

    uwnd<-get.member.at.hour('uwnd.10m',year,month,day,hour,member=1,version='3.5.1')
    if(is.null(uwnd)) stop(sprintf("Failed to load uwnd %s",sf.name))
    vwnd<-get.member.at.hour('vwnd.10m',year,month,day,hour,member=1,version='3.5.1')
    if(is.null(vwnd)) stop(sprintf("Failed to load vwnd %s",sf.name))
    t.actual<-get.member.at.hour('air.2m',year,month,day,hour,member=1,version='3.5.1')
    if(is.null(t.actual)) stop(sprintf("Failed to load t.actual %s",sf.name))
    t.normal<-t.actual
    t.normal$data[]<-rep(283,length(t.normal$data))
    s<-WeatherMap.make.streamlines(streamlines,uwnd,vwnd,t.actual,t.normal,Options)
    #save(year,month,day,hour,s,file=sf.name)
    gc(verbose=FALSE)
    return(s)

}
plot.hour<-function(year,month,day,hour,streamlines) {    


    image.name<-sprintf("%04d-%02d-%02d:%02d.png",year,month,day,hour)

    ifile.name<-sprintf("%s/%s",Imagedir,image.name)
    #if(file.exists(ifile.name) && file.info(ifile.name)$size>0) return()
    print(sprintf("%04d-%02d-%02d:%02d - %s",year,month,day,hour,
                   Sys.time()))

    prmsl<-TWCR.get.slice.at.hour('prmsl',year,month,day,hour,version='3.5.1')
    if(is.null(prmsl)) stop(sprintf("Failed to load prmsl %s",image.name))
    prmsl.spread<-TWCR.get.slice.at.hour('prmsl',year,month,day,hour,version='3.5.1',
                                              type='spread')
    if(is.null(prmsl.spread)) stop(sprintf("Failed to load prmsl.spread %s",image.name))
    prmsl.sd<-TWCR.get.slice.at.hour('prmsl',year,month,day,hour,
                                         version='3.4.1',type='standard.deviation')
    if(is.null(prmsl.sd)) stop(sprintf("Failed to load prmsl.sd %s",image.name))
    prmsl.normal<-TWCR.get.slice.at.hour('prmsl',year,month,day,hour,version='3.4.1',
                                             type='normal')
    if(is.null(prmsl.normal)) stop(sprintf("Failed to load prmsl.normal %s",image.name))
    fog.threshold<-exp(1)
    fog<-TWCR.relative.entropy(prmsl.normal,prmsl.sd,prmsl,prmsl.spread)
    fog$data[]<-1-pmin(fog.threshold,pmax(0,fog$data))/fog.threshold
    prmsl<-get.member.at.hour('prmsl',year,month,day,hour,member=1,version='3.5.1')
   
    prate<-NULL
    if(Options$show.precipitation) {
       prate<-get.member.at.hour('prate',year,month,day,hour,member=1,version='3.5.1')
       if(is.null(prate)) stop(sprintf("Failed to load prate %s",image.name))
     }
    icec<-TWCR.get.slice.at.hour('icec',year,month,day,hour,version='3.5.1')
    if(is.null(icec)) stop(sprintf("Failed to load icec %s",image.name))
    obs<-TWCR.get.obs(year,month,day,hour,version='3.5.1')
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
    dev.off()
}

s<-make.streamlines(year,month,day,hour,streamlines=NULL)
s<-make.streamlines(year,month,day,hour,streamlines=s)
s<-make.streamlines(year,month,day,hour,streamlines=s)
s<-make.streamlines(year,month,day,hour,streamlines=s)
s<-make.streamlines(year,month,day,hour,streamlines=s)
s<-make.streamlines(year,month,day,hour,streamlines=s)
s<-make.streamlines(year,month,day,hour,streamlines=s)
s<-make.streamlines(year,month,day,hour,streamlines=s)
plot.hour(year,month,day,hour,s)

