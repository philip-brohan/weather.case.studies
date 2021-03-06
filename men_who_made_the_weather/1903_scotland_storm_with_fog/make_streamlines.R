#!/usr/bin/Rscript --no-save

# Just make the streamlines for later rendering.

library(GSDF.TWCR)
library(GSDF.WeatherMap)
library(chron)

Year<-1903
Month<-2
Day<-19
Hour<-12
n.total<-9*24

c.date<-chron(dates=sprintf("%04d/%02d/%02d",Year,Month,Day),
          times=sprintf("%02d:00:00",Hour),
          format=c(dates='y/m/d',times='h:m:s'))


GSDF.cache.dir<-sprintf("%s/GSDF.cache",Sys.getenv('SCRATCH'))
if(!file.exists(GSDF.cache.dir)) dir.create(GSDF.cache.dir,recursive=TRUE)
Imagedir<-sprintf("%s/images/1903_scotland_storm",Sys.getenv('SCRATCH'))
if(!file.exists(Imagedir)) dir.create(Imagedir,recursive=TRUE)

Options<-WeatherMap.set.option(NULL)
#Options<-WeatherMap.set.option(Options,'pole.lon',145)
#Options<-WeatherMap.set.option(Options,'pole.lat',40)

range<-60
aspect<-16/9
Options<-WeatherMap.set.option(Options,'lat.min',20-5)
Options<-WeatherMap.set.option(Options,'lat.max',Options$lat.min+range+5)
Options<-WeatherMap.set.option(Options,'lon.min',-100-5)
Options<-WeatherMap.set.option(Options,'lon.max',Options$lon.min+range*aspect+5)

Options<-WeatherMap.set.option(Options,'wind.vector.points',3)
Options<-WeatherMap.set.option(Options,'wind.vector.scale',0.5)
Options<-WeatherMap.set.option(Options,'wind.vector.move.scale',25)
Options<-WeatherMap.set.option(Options,'wind.vector.density',1.5)
Options$ice.points<-100000

member=1

get.member.at.hour<-function(variable,year,month,day,hour,member,version='3.5.1') {

       t<-TWCR.get.members.slice.at.hour(variable,year,month,day,
                                  hour,version=version)
       t<-GSDF.select.from.1d(t,'ensemble',member)
       gc()
       return(t)
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

    uwnd<-get.member.at.hour('uwnd.10m',year,month,day,hour,
                                  member=member,version='3.5.1')
    vwnd<-get.member.at.hour('vwnd.10m',year,month,day,hour,
                                  member=member,version='3.5.1')
    t.actual<-get.member.at.hour('air.2m',year,month,day,hour,
                                  member=member,version='3.5.1')
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
