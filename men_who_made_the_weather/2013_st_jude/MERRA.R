#!/usr/common/graphics/R/R-3.1.0/bin/R --no-save

# 2013 St Jude's day storm

library(GSDF.MERRA)
library(GSDF.TWCR)
library(GSDF.WeatherMap)
library(parallel)

Year<-2013
Month<-10
Day<-26
Hour<-0
n.total<-24*6*4 # Total number of timesteps to be rendered
version<-'3.5.1'
fog.threshold<-exp(1)

GSDF.cache.dir<-"/scratch/hadpb/GSDF.cache"
if(!file.exists(GSDF.cache.dir)) dir.create(GSDF.cache.dir,recursive=TRUE)
Imagedir<-"/scratch/hadpb/images/2013_st_jude_merra"
if(!file.exists(Imagedir)) dir.create(Imagedir,recursive=TRUE)

c.date<-chron(dates=sprintf("%04d/%02d/%02d",Year,Month,Day),
          times=sprintf("%02d:00:00",Hour),
          format=c(dates='y/m/d',times='h:m:s'))

Options<-WeatherMap.set.option(NULL)
Options<-WeatherMap.set.option(Options,'show.mslp',T)
Options<-WeatherMap.set.option(Options,'show.ice',F)
Options<-WeatherMap.set.option(Options,'show.obs',F)
Options<-WeatherMap.set.option(Options,'show.fog',F)
Options<-WeatherMap.set.option(Options,'show.precipitation',F)
Options<-WeatherMap.set.option(Options,'temperature.range',6)

range<-15
aspect<-4/3
Options<-WeatherMap.set.option(Options,'lat.min',range*-1)
Options<-WeatherMap.set.option(Options,'lat.max',range)
Options<-WeatherMap.set.option(Options,'lon.min',range*aspect*-1)
Options<-WeatherMap.set.option(Options,'lon.max',range*aspect)
Options<-WeatherMap.set.option(Options,'pole.lon',173)
Options<-WeatherMap.set.option(Options,'pole.lat',36)

land<-WeatherMap.get.land(Options)
Options$wind.vector.lwd<-3
Options$wind.vector.scale<-0.5
Options$wind.vector.density<-1.5
Options$wind.vector.move.scale<-10

Options$mslp.lwd<-3
Options$mslp.base=0                    # Base value for anomalies
Options$mslp.range=50000                    # Anomaly for max contour
Options$mslp.step=250                       # Smaller -more contours
Options$mslp.tpscale=3500                    # Smaller -contours less transparent

get.member.at.hour<-function(variable,year,month,day,hour) {

       t<-MERRA.get.slice.at.hour(variable,year,month,day,hour)
       gc()
       return(t)
  }


make.streamlines<-function(year,month,day,hour,streamlines=NULL,count) {

    sf.name<-sprintf("%s/streamlines.%04d-%02d-%02d:%02d:%02d.rd",
                           Imagedir,year,month,day,as.integer(hour),
                                               as.integer(hour%%1*60))
    if(count>0 && file.exists(sf.name) && file.info(sf.name)$size>500000) {
       load(sf.name)
       return(s)
    }
    print(sprintf("%04d-%02d-%02d:%02d:%02d - %s",year,month,day,as.integer(hour),
                   as.integer(hour%%1*60),
                   Sys.time()))

    uwnd<-get.member.at.hour('U10M',year,month,day,hour)
    vwnd<-get.member.at.hour('V10M',year,month,day,hour)
    t.actual<-get.member.at.hour('T2M',year,month,day,hour)
    t.normal<-t.actual
    t.normal$data[]<-rep(283,length(t.normal$data))
    s<-WeatherMap.make.streamlines(streamlines,uwnd,vwnd,t.actual,t.normal,Options)
    save(year,month,day,hour,s,file=sf.name)
    gc(verbose=FALSE)
    return(s)

}

plot.hour<-function(year,month,day,hour,streamlines) {    

    image.name<-sprintf("%04d-%02d-%02d:%02d:%02d.png",year,month,day,as.integer(hour),
                                                         as.integer(hour%%1*60))
    ifile.name<-sprintf("%s/%s",Imagedir,image.name)
    if(file.exists(ifile.name) && file.info(ifile.name)$size>0) return()

    prmsl<-get.member.at.hour('SLP',year,month,day,hour)
    prmsl.n<-TWCR.get.slice.at.hour('prmsl',year,month,day,hour,version='3.4.1',type='normal')
    prmsl.n<-GSDF.regrid.2d(prmsl.n,prmsl)
    prmsl$data[]<-as.vector(prmsl$data)-as.vector(prmsl.n$data)

     png(ifile.name,
             width=1080*WeatherMap.aspect(Options),
             height=1080,
             bg=Options$sea.colour,
             pointsize=24,
             type='cairo')
    Options$label<-sprintf("%04d-%02d-%02d:%02d",year,month,day,as.integer(hour))
    tryCatch({
       WeatherMap.draw(Options=Options,uwnd=NULL,icec=NULL,
                          vwnd=NULL,precip=NULL,mslp=prmsl,
                          t.actual=NULL,t.normal=NULL,land=land,
                          fog=NULL,obs=NULL,streamlines=streamlines)
              },
              error=function(cond) {
                message(cond)
                # Choose a return value in case of error
                return(NA)
              },
              warning=function(cond) {
                message(cond)
                # Choose a return value in case of warning
                return(NULL)
             },
             finally={}
    )    

    dev.off()
}

s<-NULL
for(n.count in seq(0,n.total)) {

    n.date<-c.date+n.count/(24*6) # 10-minute timesteps
    year<-as.numeric(as.character(years(n.date)))
    month<-months(n.date)
    day<-days(n.date)
    hour<-hours(n.date)+minutes(n.date)/60

    # serial component - streamlines evolve from hour to hour
    s<-make.streamlines(year,month,day,hour,streamlines=s,n.count)
    if(n.count==0) {
      s$status<-s$status*0+4
      s<-make.streamlines(year,month,day,hour,streamlines=s,n.count)
      s<-make.streamlines(year,month,day,hour,streamlines=s,n.count)
      s<-make.streamlines(year,month,day,hour,streamlines=s,n.count)
      s<-make.streamlines(year,month,day,hour,streamlines=s,n.count)
      s<-make.streamlines(year,month,day,hour,streamlines=s,n.count)
      s<-make.streamlines(year,month,day,hour,streamlines=s,n.count)
    }
    image.name<-sprintf("%04d-%02d-%02d:%02d:%02d.png",year,month,day,as.integer(hour),
                                                         as.integer(hour%%1*60))
    ifile.name<-sprintf("%s/%s",Imagedir,image.name)
    if(file.exists(ifile.name) && file.info(ifile.name)$size>0) next
    # Sequential - don't overload the openDAP servers
    plot.hour(year,month,day,hour,s)

}
