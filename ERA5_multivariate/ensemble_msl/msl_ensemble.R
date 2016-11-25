#!/usr/bin/Rscript --no-save

# Spaghetti-contour plot of the ERA5 msl ensemble

library(GSDF.ERA5)
library(GSDF.ERAI)
library(GSDF.WeatherMap)
library(grid)
library(getopt)

opt = getopt(c(
  'year',   'y', 2, "integer",
  'month',  'm', 2, "integer",
  'day',    'd', 2, "integer",
  'hour',   'h', 2, "numeric"
))
if ( is.null(opt$year) )   { stop("Year not specified") }
if ( is.null(opt$month) )  { stop("Month not specified") }
if ( is.null(opt$day) )    { stop("Day not specified") }
if ( is.null(opt$hour) )   { stop("Hour not specified") }

Imagedir<-sprintf("%s/images/ERA5_msl_ensemble",Sys.getenv('SCRATCH'))
if(!file.exists(Imagedir)) dir.create(Imagedir,recursive=TRUE)

Options<-WeatherMap.set.option(NULL)
#Options<-WeatherMap.set.option(Options,'land.colour',rgb(100,100,100,255,
#                                                       maxColorValue=255))
#Options<-WeatherMap.set.option(Options,'sea.colour',rgb(150,150,150,255,
#                                                       maxColorValue=255))
#Options<-WeatherMap.set.option(Options,'ice.colour',rgb(250,250,250,255,
#                                                       maxColorValue=255))
Options<-WeatherMap.set.option(Options,'background.resolution','high')
Options<-WeatherMap.set.option(Options,'pole.lon',160)
Options<-WeatherMap.set.option(Options,'pole.lat',45)

Options<-WeatherMap.set.option(Options,'lat.min',-90)
Options<-WeatherMap.set.option(Options,'lat.max',90)
Options<-WeatherMap.set.option(Options,'lon.min',-140)
Options<-WeatherMap.set.option(Options,'lon.max',240)
Options$vp.lon.min<- -130
Options$vp.lon.max<-  230
Options<-WeatherMap.set.option(Options,'wrap.spherical',F)

Options<-WeatherMap.set.option(Options,'wind.vector.points',3)
Options<-WeatherMap.set.option(Options,'wind.vector.scale',0.1)
Options<-WeatherMap.set.option(Options,'wind.vector.move.scale',1)
Options<-WeatherMap.set.option(Options,'wind.vector.density',1)
Options$ice.points<-100000

Options$mslp.base=0                    # Base value for anomalies
Options$mslp.range=50000                    # Anomaly for max contour
Options$mslp.step=500                       # Smaller -> more contours
Options$mslp.tpscale=500                    # Smaller -> contours less transparent
Options$mslp.lwd=1
Options$precip.colour=c(0,0.2,0)

contour.levels<-seq(-300,300,30)
contour.levels<-abs(contour.levels)**1.5*sign(contour.levels)

Draw.pressure<-function(mslp,Options,colour=c(0,0,0)) {

  M<-GSDF.WeatherMap:::WeatherMap.rotate.pole(mslp,Options)
  M<-GSDF:::GSDF.pad.longitude(M) # Extras for periodic boundary conditions
  lats<-M$dimensions[[GSDF.find.dimension(M,'lat')]]$values
  longs<-M$dimensions[[GSDF.find.dimension(M,'lon')]]$values
    # Need particular data format for contourLines
  maxl<-Options$vp.lon.max+2
  if(lats[2]<lats[1] || longs[2]<longs[1] || max(longs) > maxl ) {
    if(lats[2]<lats[1]) lats<-rev(lats)
    if(longs[2]<longs[1]) longs<-rev(longs)
    longs[longs>maxl]<-longs[longs>maxl]-(maxl*2)
    longs<-sort(longs)
    M2<-M
    M2$dimensions[[GSDF.find.dimension(M,'lat')]]$values<-lats
    M2$dimensions[[GSDF.find.dimension(M,'lon')]]$values<-longs
    M<-GSDF.regrid.2d(M,M2)
  }
  z<-matrix(data=M$data,nrow=length(longs),ncol=length(lats))
  #contour.levels<-seq(Options$mslp.base-Options$mslp.range,
  #                    Options$mslp.base+Options$mslp.range,
  #                    Options$mslp.step)
  lines<-contourLines(longs,lats,z,
                       levels=contour.levels)
  if(!is.na(lines) && length(lines)>0) {
     for(i in seq(1,length(lines))) {
         tp<-min(1,(abs(lines[[i]]$level-Options$mslp.base)/
                    Options$mslp.tpscale))
         lt<-2
         lwd<-1
         if(lines[[i]]$level<=Options$mslp.base) {
             lt<-1
             lwd<-1
         }
         gp<-gpar(col=rgb(colour[1],colour[2],colour[3],tp),
                             lwd=Options$mslp.lwd*lwd,lty=lt)
         res<-tryCatch({
             grid.xspline(x=unit(lines[[i]]$x,'native'),
                        y=unit(lines[[i]]$y,'native'),
                        shape=1,
                        gp=gp)
             }, warning = function(w) {
                 print(w)
             }, error = function(e) {
                print(e)
             }, finally = {
                # Do nothing
             })
     }
  }
}

plot.hour<-function(year,month,day,hour) {

    image.name<-sprintf("%04d-%02d-%02d:%02d.%02d.png",year,month,day,as.integer(hour),
                        as.integer((hour%%1)*100))

    ifile.name<-sprintf("%s/%s",Imagedir,image.name)
    if(file.exists(ifile.name) && file.info(ifile.name)$size>0) return()

    prmsl.e<-ERA5.get.members.slice.at.hour('prmsl',year,month,day,hour)
    em<-GSDF.select.from.1d(prmsl.e,'ensemble',1)
    prn<-readRDS(sprintf("%s/ERA5/oper/climtologies.test/prmsl.%02d.Rdata",
                           Sys.getenv('SCRATCH'),as.integer(hour)))
    hour.fraction<-hour-as.integer(hour)
    if(hour.fraction>0) {
      yp<-ymd_hms(sprintf("%04d-%02d-%02d:%02d:00:00",year,month,day,as.integer(hour)))+hours(1)
      prn2<-readRDS(sprintf("%s/ERA5/oper/climtologies.test/prmsl.%02d.Rdata",
                           Sys.getenv('SCRATCH'),lubridate::hour(yp)))
      prn$data[]<-prn$data*(1-hour.fraction)+prn2$data*hour.fraction
    }
    prn<-GSDF.regrid.2d(prn,em)

    land<-WeatherMap.get.land(Options)
     png(ifile.name,
             width=1080*16/9,
             height=1080,
             bg=Options$sea.colour,
             pointsize=24,
             type='cairo')
  base.gp<-gpar(family='Helvetica',font=1,col='black')
  lon.min<-Options$lon.min
  if(!is.null(Options$vp.lon.min)) lon.min<-Options$vp.lon.min
  lon.max<-Options$lon.max
  if(!is.null(Options$vp.lon.max)) lon.max<-Options$vp.lon.max
  lat.min<-Options$lat.min
  if(!is.null(Options$vp.lat.min)) lat.min<-Options$vp.lat.min
  lat.max<-Options$lat.max
  if(!is.null(Options$vp.lat.max)) lat.max<-Options$vp.lat.max
  pushViewport(dataViewport(c(lon.min,lon.max),c(lat.min,lat.max),
		            extension=0,gp=base.gp))
    
      WeatherMap.draw.land(land,Options)
    for(e in seq(1,10)) {
      em<-GSDF.select.from.1d(prmsl.e,'ensemble',e)
      em$data[]<-em$data-prn$data
      Draw.pressure(em,Options,colour=c(0,0,0))
    }
    Options$label=sprintf("%04d-%02d-%02d:%02d",year,month,day,as.integer(hour))
    WeatherMap.draw.label(Options)
    dev.off()
}

plot.hour(opt$year,opt$month,opt$day,opt$hour)

