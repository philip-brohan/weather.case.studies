#!/usr/bin/env Rscript 

# Wind, ice, pressure, temperature and precip polyhedra.
# Just do the rendering - use pre-calculated streamlines
# Render just one timestep - parallelise on SPICE.
# Sub-hourly version - fudge streamlines

library(GSDF.TWCR)
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

Imagedir<-sprintf("%s/images/1903_scotland_storm",Sys.getenv('SCRATCH'))
Stream.dir<-sprintf("%s/images/1903_scotland_storm",Sys.getenv('SCRATCH'))
if(!file.exists(Imagedir)) dir.create(Imagedir,recursive=TRUE)

Options<-WeatherMap.set.option(NULL)
Options<-WeatherMap.set.option(Options,'land.colour',rgb(100,100,100,255,
                                                       maxColorValue=255))
Options<-WeatherMap.set.option(Options,'sea.colour',rgb(150,150,150,255,
                                                       maxColorValue=255))
Options<-WeatherMap.set.option(Options,'ice.colour',rgb(250,250,250,255,
                                                       maxColorValue=255))
#Options<-WeatherMap.set.option(Options,'background.resolution','high')
#Options<-WeatherMap.set.option(Options,'pole.lon',145)
#Options<-WeatherMap.set.option(Options,'pole.lat',40)

range<-60
aspect<-16/9
Options<-WeatherMap.set.option(Options,'lat.min',20)
Options<-WeatherMap.set.option(Options,'lat.max',Options$lat.min+range)
Options<-WeatherMap.set.option(Options,'lon.min',-100)
Options<-WeatherMap.set.option(Options,'lon.max',Options$lon.min+range*aspect)
Options<-WeatherMap.set.option(Options,'wrap.spherical',F)
Options$vp.lon.min<- Options$lon.min
Options$vp.lon.max<- Options$lon.max

Options<-WeatherMap.set.option(Options,'wind.vector.points',3)
Options<-WeatherMap.set.option(Options,'wind.vector.scale',0.5)
Options<-WeatherMap.set.option(Options,'wind.vector.move.scale',25)
Options<-WeatherMap.set.option(Options,'wind.vector.density',1.5)
Options$ice.points<-100000

Options$mslp.base=0                    # Base value for anomalies
Options$mslp.range=50000                    # Anomaly for max contour
Options$mslp.step=500                       # Smaller -> more contours
Options$mslp.tpscale=3500                    # Smaller -> contours less transparent
Options$mslp.lwd=2
Options$precip.colour=c(0,0.2,0)
Options$label.xp=0.995

member=1

get.member.at.hour<-function(variable,year,month,day,hour,member,version='3.5.1') {

       t<-TWCR.get.members.slice.at.hour(variable,year,month,day,
                                  hour,version=version)
       t<-GSDF.select.from.1d(t,'ensemble',member)
       gc()
       return(t)
  }
get.mean.at.hour<-function(variable,year,month,day,hour,version='3.5.1') {

       t<-TWCR.get.members.slice.at.hour(variable,year,month,day,
                                  hour,version=version)
       t<-GSDF.reduce.1d(t,'ensemble',mean)
       gc()
       return(t)
  }

WeatherMap.streamline.getGC<-function(value,transparency=NA,status=1,Options) {
   alpha<-c(10,50,150,255)[min(status,4)]
   return(gpar(col=rgb(125,125,125,alpha,maxColorValue=255),
               fill=rgb(125,125,125,alpha,maxColorValue=255),lwd=Options$wind.vector.lwd))
}
assignInNamespace("WeatherMap.streamline.getGC",WeatherMap.streamline.getGC, ns="GSDF.WeatherMap")

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
  contour.levels<-seq(Options$mslp.base-Options$mslp.range,
                      Options$mslp.base+Options$mslp.range,
                      Options$mslp.step)
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

get.streamlines<-function(year,month,day,hour) {

    hour.fraction<-hour-as.integer(hour)
    sf.name<-sprintf("%s/streamlines.%04d-%02d-%02d:%02d.rd",
                           Stream.dir,year,month,day,as.integer(hour))
    if(file.exists(sf.name) && file.info(sf.name)$size>5000) {
       load(sf.name)
       # Fudge the streamlines for the fractional hour
       s.old<-s
       if(hour.fraction>0) {
          x.range<-c(Options$lon.min,Options$lon.max)
          y.range<-c(Options$lat.min,Options$lat.max)
          view.scale<-max(diff(x.range)/360,diff(y.range)/180)
          move.scale<-0.033*Options$wind.vector.points/Options$wind.vector.scale
          move.scale<-move.scale*Options$wind.vector.move.scale*view.scale
          print(move.scale)
          for(p in seq(1,Options$wind.vector.points)) {
           s[['x']][,p]<-s.old[['x']][,p]+
                        (s.old[['x']][,2]-s.old[['x']][,1])*move.scale*hour.fraction
           s[['y']][,p]<-s.old[['y']][,p]+
                        (s.old[['y']][,2]-s.old[['y']][,1])*move.scale*hour.fraction
         }
       }
       return(s)
    } else {
      stop(sprintf("No streamlines available for %04d-%02d-%02d:%02d",
                   year,month,day,hour))
    }

}

plot.hour<-function(year,month,day,hour,streamlines) {

    image.name<-sprintf("%04d-%02d-%02d:%02d.%02d.png",year,month,day,as.integer(hour),
                        as.integer((hour%%1)*100))

    ifile.name<-sprintf("%s/%s",Imagedir,image.name)
    if(file.exists(ifile.name) && file.info(ifile.name)$size>0) return()

    land<-WeatherMap.get.land(Options)
    
    t2m<-get.mean.at.hour('air.2m',year,month,day,hour,version='3.5.1')
    t2n<-TWCR.get.slice.at.hour('air.2m',year,month,day,hour,type='normal',version='3.4.1')
    t2n<-GSDF.regrid.2d(t2n,t2m)
    t2m$data[]<-t2m$data-t2n$data
    prmsl.T<-get.mean.at.hour('prmsl',year,month,day,hour,version='3.5.1')
    prn<-prmsl.T
    prn$data[]<-prn$data*0+101325
    prmsl.T$data[]<-prmsl.T$data-prn$data
    #icec<-TWCR.get.slice.at.hour('icec',year,month,day,hour,version='3.5.1')
    prate<-get.member.at.hour('prate',year,month,day,hour,member,version='3.5.1')
  
     png(ifile.name,
             width=1080*aspect,
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
    
      ip<-WeatherMap.rectpoints(Options$ice.points,Options)
      #WeatherMap.draw.ice(ip$lat,ip$lon,icec,Options)
      WeatherMap.draw.land(land,Options)
      WeatherMap.draw.streamlines(streamlines,Options)
       Draw.temperature(t2m,Options,Trange=10)
       WeatherMap.draw.precipitation(prate,Options)
    Draw.pressure(prmsl.T,Options,colour=c(0,0,0))
    Options$label=sprintf("%04d-%02d-%02d:%02d",year,month,day,as.integer(hour))
    WeatherMap.draw.label(Options)
    dev.off()
}

# Use pre-calculated streamlines
s<-get.streamlines(opt$year,opt$month,opt$day,opt$hour)
plot.hour(opt$year,opt$month,opt$day,opt$hour,s)

