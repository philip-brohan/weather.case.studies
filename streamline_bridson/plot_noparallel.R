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
Options$wind.vector.lwd<-2
Options$wind.vector.move.scale<-Options$wind.vector.move.scale/3
Options$wind.vector.density<-Options$wind.vector.density*0.5
Options$wind.vector.scale<-Options$wind.vector.scale*2
Options$wind.vector.points<-7
land<-WeatherMap.get.land(Options)

Options<-WeatherMap.set.option(Options,'cores',1)
Options<-WeatherMap.set.option(Options,'bridson.subsample',1)


plot.hour<-function(year,month,day,hour,streamlines,ifile.name) {

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

    uwnd<-TWCR.get.slice.at.hour('uwnd.10m',year,month,day,hour,version=version)
    #uwnd$data[]<-rep(10,length(uwnd$data))
    vwnd<-TWCR.get.slice.at.hour('vwnd.10m',year,month,day,hour,version=version)
    #vwnd$data[]<-rep(0,length(vwnd$data))    
    t.actual<-TWCR.get.slice.at.hour('air.2m',year,month,day,hour,version=version)
    t.normal<-t.actual
    t.normal$data[]<-rep(286,length(t.normal$data))
    s<-WeatherMap.make.streamlines(s,uwnd,vwnd,t.actual,t.normal,Options)
    # set the status to make all the lines visible
    s$status<-s$status*0+4
    ifile1.name<-sprintf("%s/%s",Imagedir,'pass1.png')
    plot.hour(year,month,day,hour,s,ifile1.name)
 
    # Twice round to include the downstream points
    s<-NULL
    for(i in seq(1,25)) {
       s<-WeatherMap.make.streamlines(s,uwnd,vwnd,t.actual,t.normal,Options)
    }
    s$status<-s$status*0+4
    ifile2.name<-sprintf("%s/%s",Imagedir,'pass2.png')
    plot.hour(year,month,day,hour,s,ifile2.name)

    # Make side-by-side comparison
    f1<-readPNG(ifile1.name)
    f2<-readPNG(ifile2.name)
    d<-dim(f1)
    for(chan in seq(1,d[3])) {
       for(row in seq(1,d[1])) {
          f1[row,,chan]<-c(f1[row,1:(d[2]/2),chan],f2[row,1:(d[2]/2),chan])
       }
    }
    writePNG(f1,'sbs.png')
    
