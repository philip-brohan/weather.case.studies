#!/usr/bin/Rscript --no-save

# Show wind and temperature in 20CR2c
# Spherical version with garish colours
# Move the north pole to centre UK - shows up better on PufferSphere?
# Single step

library(GSDF.TWCR)
library(GSDF.WeatherMap)
library(chron)
library(parallel)
library(getopt)

opt = getopt(c(
  'date',   'd', 2, "character",
  'step',   's', 2, "integer"
));
if ( is.null(opt$date) )   { stop("Date not specified") }
if ( is.null(opt$step) )   { stop("Step not specified") }

version<-'3.5.1'

Imagedir<-sprintf("/scratch/hadpb/images/TWCR_spherical_obliquity_rotate/")
if(!file.exists(Imagedir)) dir.create(Imagedir,recursive=TRUE)


Options<-WeatherMap.set.option(NULL)
Options<-WeatherMap.set.option(Options,'lat.min',-90)
Options<-WeatherMap.set.option(Options,'lat.max',90)
Options<-WeatherMap.set.option(Options,'lon.min',-190)
Options<-WeatherMap.set.option(Options,'lon.max',190)
Options$vp.lon.min<- -180
Options$vp.lon.max<-  180
Options<-WeatherMap.set.option(Options,'jitter',FALSE)
Options<-WeatherMap.set.option(Options,'wrap.spherical',T)
Options<-WeatherMap.set.option(Options,'show.mslp',F)
Options<-WeatherMap.set.option(Options,'show.ice',F)
Options<-WeatherMap.set.option(Options,'show.obs',F)
Options<-WeatherMap.set.option(Options,'show.fog',F)
Options<-WeatherMap.set.option(Options,'show.precipitation',F)
Options<-WeatherMap.set.option(Options,'temperature.range',25)
Options<-WeatherMap.set.option(Options,'wind.palette',
                       diverge_hcl(7,c=c(150,0),l=c(25,30),power=1))
Options<-WeatherMap.set.option(Options,'land.colour',rgb(125,125,125,255,
                                                       maxColorValue=255))
Options<-WeatherMap.set.option(Options,'sea.colour',rgb(200,200,200,255,
                                                       maxColorValue=255))
Options<-WeatherMap.set.option(Options,'pole.lon',160)
Options<-WeatherMap.set.option(Options,'pole.lat',35)
Options<-WeatherMap.set.option(Options,'background.resolution','high')

aspect<-2

Options$ice.points<-100000
Options$wind.vector.lwd<-2.5
Options$wind.vector.move.scale<-Options$wind.vector.move.scale/3
Options$wind.vector.density<-Options$wind.vector.density*0.5

set.pole<-function(step,Options) {
  lon<-160+(step/10)
  if(lon>360) lon<-lon%%360
  lat<-90+sin(step/500)*89
  Options<-WeatherMap.set.option(Options,'pole.lon',lon)
  Options<-WeatherMap.set.option(Options,'pole.lat',lat)
  return(Options)
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

plot.hour<-function(date.string,step,streamlines) {


    image.name<-sprintf("%s.png",date.string)

    ifile.name<-sprintf("%s/%s",Imagedir,image.name)
    if(file.exists(ifile.name) && file.info(ifile.name)$size>0) return()

    Options<-set.pole(step,Options)
    land<-WeatherMap.get.land(Options)
    
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
    
   system(sprintf("mogrify -gamma 0.6 %s",ifile.name))
}

s<-make.streamlines(opt$date)
plot.hour(opt$date,opt$step,s)
