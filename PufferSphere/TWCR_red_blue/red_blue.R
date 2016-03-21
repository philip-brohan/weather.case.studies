#!/usr/common/usg/R/R-3.0.1/bin/Rscript --no-save

# Near-surface temperature anomalies - idealised

library(GSDF.TWCR)
library(GSDF.WeatherMap)
library(parallel)
library(getopt)


Year<-2014
Month<-1
Day<-1
Hour<-0
n.total<-24*365 # Total number of hours to be rendered
version<-'3.5.1'

GSDF.cache.dir<-sprintf("%s/GSDF.cache",Sys.getenv('GSCRATCH'))
Imagedir<-sprintf("%s/images/TWCR_red_blue",Sys.getenv('GSCRATCH'))
if(!file.exists(Imagedir)) dir.create(Imagedir,recursive=TRUE)

c.date<-chron(dates=sprintf("%04d/%02d/%02d",Year,Month,Day),
          times=sprintf("%02d:00:00",Hour),
          format=c(dates='y/m/d',times='h:m:s'))

Options<-WeatherMap.set.option(NULL)
aspect<-2
Options<-WeatherMap.set.option(Options,'lat.min',-90)
Options<-WeatherMap.set.option(Options,'lat.max',90)
Options<-WeatherMap.set.option(Options,'lon.min',-190)
Options<-WeatherMap.set.option(Options,'lon.max',190)
Options$vp.lon.min<- -180
Options$vp.lon.max<-  180
Options<-WeatherMap.set.option(Options,'pole.lon',160)
Options<-WeatherMap.set.option(Options,'pole.lat',35)
Options<-WeatherMap.set.option(Options,'background.resolution','high')

Draw.temperature<-function(temperature,Options,Trange=1) {
  
  Options.local<-Options
  Options.local$fog.min.transparency<-0.5
  tplus<-temperature
  tplus$data[]<-pmax(0,pmin(Trange,tplus$data))/Trange
  Options.local$fog.colour<-c(1,0,0)
  WeatherMap.draw.fog(tplus,Options.local)
  tminus<-temperature
  tminus$data[]<-tminus$data*-1
  tminus$data[]<-pmax(0,pmin(Trange,tminus$data))/Trange
  Options.local$fog.colour<-c(0,0,1)
  WeatherMap.draw.fog(tminus,Options.local)
}


plot.hour<-function(year,month,day,hour) {    

    image.name<-sprintf("%04d-%02d-%02d:%02d.png",year,month,day,hour)

    ifile.name<-sprintf("%s/%s",Imagedir,image.name)
    if(file.exists(ifile.name) && file.info(ifile.name)$size>0) return()

    t2m<-TWCR.get.slice.at.hour('air.2m',year,month,day,hour,version=version)
    t2m.normal<-TWCR.get.slice.at.hour('air.2m',year,month,day,hour,
                                             type='normal',version='3.4.1')
    t2m$data[]<-t2m$data-t2m.normal$data
    
     png(ifile.name,
             width=1050*aspect,
             height=1050,
             bg='black',
             pointsize=24,
             type='cairo')
       pushViewport(dataViewport(c(Options$vp.lon.min,Options$vp.lon.max),
                                 c(Options$lat.min,Options$lat.max),
                                  extension=0))

        Draw.temperature(t2m,Options)
    upViewport()

    dev.off()
}

for(n.count in seq(0,n.total)) {

    n.date<-c.date+n.count/24
    year<-as.numeric(as.character(years(n.date)))
    month<-months(n.date)
    day<-days(n.date)
    hour<-n.count%%24

    image.name<-sprintf("%04d-%02d-%02d:%02d.png",year,month,day,hour)
    ifile.name<-sprintf("%s/%s",Imagedir,image.name)
    if(file.exists(ifile.name) && file.info(ifile.name)$size>0) next
    # Each plot in a seperate parallel process
    #mcparallel(plot.hour(year,month,day,hour))
    plot.hour(year,month,day,hour)
    if(n.count%%6==0) mccollect(wait=TRUE)

}
mccollect()
