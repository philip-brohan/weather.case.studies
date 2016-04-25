#!/usr/bin/Rscript

# 1976 drought

library(GSDF.TWCR)
library(GSDF.WeatherMap)
library(parallel)

library(getopt)

opt = getopt(c(
  'date',   'd', 2, "character"
));
if ( is.null(opt$date) )   { stop("Date not specified") }

version<-'3.5.1'

Imagedir<-sprintf("/scratch/hadpb/images/1976-summer/")
if(!file.exists(Imagedir)) dir.create(Imagedir,recursive=TRUE)

Options<-WeatherMap.set.option(NULL)
Options<-WeatherMap.set.option(Options,'show.mslp',T)
Options<-WeatherMap.set.option(Options,'show.ice',F)
Options<-WeatherMap.set.option(Options,'show.obs',F)
Options<-WeatherMap.set.option(Options,'show.fog',F)
Options<-WeatherMap.set.option(Options,'show.precipitation',T)
Options<-WeatherMap.set.option(Options,'temperature.range',6)
Options<-WeatherMap.set.option(Options,'precip.points',50000)
Options<-WeatherMap.set.option(Options,'precip.range',0.015)
Options<-WeatherMap.set.option(Options,'precip.min.transparency',0.85)

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

get.member.at.hour<-function(variable,year,month,day,hour,member) {

       t<-TWCR.get.members.slice.at.hour(variable,year,month,day,
                                  hour,version=version)
       t<-GSDF.select.from.1d(t,'ensemble',member)
       gc()
       return(t)
  }

make.streamlines<-function(date.string) {

    sf.name<-sprintf("%s/streamlines.%s.rd",Imagedir,
                           date.string)
    if(file.exists(sf.name) && file.info(sf.name)$size>5000) {
       load(sf.name)
       return(s)
    }
    stop(cat("Missing streamlines file",sf.name,"\n"))

}

plot.hour<-function(date.string,streamlines) {

    image.name<-sprintf("%s.png",date.string)
    ifile.name<-sprintf("%s/%s",Imagedir,image.name)
    if(file.exists(ifile.name) && file.info(ifile.name)$size>0) return()

    year<-as.integer(substr(date.string,1,4))
    month<-as.integer(substr(date.string,6,7))
    day<-as.integer(substr(date.string,9,10))
    hour<-as.integer(substr(date.string,12,13))+
          as.integer(substr(date.string,15,16))/60

    prmsl<-get.member.at.hour('prmsl',year,month,day,hour,member=1)
    prmsl.n<-TWCR.get.slice.at.hour('prmsl',year,month,day,hour,version='3.4.1',type='normal')
    prmsl.n<-GSDF.regrid.2d(prmsl.n,prmsl)
    prmsl$data[]<-as.vector(prmsl$data)-as.vector(prmsl.n$data)
    prate<-get.member.at.hour('prate',year,month,day,hour,member=1)

     png(ifile.name,
             width=1080*WeatherMap.aspect(Options),
             height=1080,
             bg=Options$sea.colour,
             pointsize=24,
             type='cairo')
    Options$label<-date.string
       WeatherMap.draw(Options=Options,uwnd=NULL,icec=NULL,
                          vwnd=NULL,precip=prate,mslp=prmsl,
                          t.actual=NULL,t.normal=NULL,land=land,
                          fog=NULL,obs=NULL,streamlines=streamlines)

    dev.off()
}

s<-make.streamlines(opt$date)
plot.hour(opt$date,s)
