#!/usr/bin/env Rscript 

# Wind, ice, pressure, temperature and precip polyhedra.
# Just do the rendering - use pre-calculated streamlines
# Render just one timestep - parallelise on SPICE.
# Sub-hourly version - fudge streamlines

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

Imagedir<-sprintf("%s/images/ERAI_multivariate.1987",Sys.getenv('SCRATCH'))
Stream.dir<-sprintf("%s/images/ERAI_multivariate.1987",Sys.getenv('SCRATCH'))
if(!file.exists(Imagedir)) dir.create(Imagedir,recursive=TRUE)

Options<-WeatherMap.set.option(NULL)
Options<-WeatherMap.set.option(Options,'land.colour',rgb(100,100,100,255,
                                                       maxColorValue=255))
Options<-WeatherMap.set.option(Options,'sea.colour',rgb(150,150,150,255,
                                                       maxColorValue=255))
Options<-WeatherMap.set.option(Options,'ice.colour',rgb(250,250,250,255,
                                                       maxColorValue=255))
Options<-WeatherMap.set.option(Options,'background.resolution','high')
range<-15
aspect<-4/3
Options<-WeatherMap.set.option(Options,'lat.min',range*-1)
Options<-WeatherMap.set.option(Options,'lat.max',range)
Options<-WeatherMap.set.option(Options,'lon.min',range*aspect*-1)
Options<-WeatherMap.set.option(Options,'lon.max',range*aspect)
Options<-WeatherMap.set.option(Options,'pole.lon',173)
Options<-WeatherMap.set.option(Options,'pole.lat',36)
Options<-WeatherMap.set.option(Options,'wrap.spherical',F)

Options<-WeatherMap.set.option(Options,'wind.vector.points',3)
Options<-WeatherMap.set.option(Options,'wind.vector.scale',0.5)
Options<-WeatherMap.set.option(Options,'wind.vector.move.scale',30/4)
Options<-WeatherMap.set.option(Options,'wind.vector.density',1.5)
Options$ice.points<-100000

Options$mslp.base=101325                    # Base value for anomalies
Options$mslp.range=100000                    # Anomaly for max contour
Options$mslp.step=300                       # Smaller -> more contours
Options$mslp.tpscale=500                    # Smaller -> contours less transparent
Options$mslp.lwd=1
Options$precip.colour=c(0,0.2,0)
Options$label.xp=0.995

# Load the 0.25 degree orography
orog<-GSDF.ncdf.load(sprintf("%s/orography/elev.0.25-deg.nc",Sys.getenv('SCRATCH')),'data',
                             lat.range=c(-90,90),lon.range=c(-180,360))
orog<-GSDF.select.from.1d(orog,'time',1)
orog$data[orog$data<0]<-0 # sea-surface, not sea-bottom
is.na(orog$data[orog$data==0])<-TRUE
# 1-km orography (slow, needs lots of ram) - only load a slice
if(TRUE) {
    orog<-GSDF.ncdf.load(sprintf("%s/orography/ETOPO2v2c_ud_rl.nc",Sys.getenv('SCRATCH')),'z',
                                 lat.range=c(-90,90),lon.range=c(0,360))
    orog$data[orog$data<0]<-0 # sea-surface, not sea-bottom
    is.na(orog$data[orog$data==0])<-TRUE
}
# Plot the orography - raster background, fast
draw.land.flat<-function(Options) {
   land<-GSDF.WeatherMap:::WeatherMap.rotate.pole(GSDF:::GSDF.pad.longitude(orog),Options)
   lons<-land$dimensions[[GSDF.find.dimension(land,'lon')]]$values
   base.colour<-Options$land.colour
   plot.colours<-rep(rgb(0,0,0,0),length(land$data))
      w<-which(land$data>0)
      plot.colours[w]<-base.colour
      m<-matrix(plot.colours, ncol=length(lons), byrow=TRUE)
      r.w<-max(lons)-min(lons)+(lons[2]-lons[1])
      r.c<-(max(lons)+min(lons))/2
      grid.raster(m,,
                   x=unit(r.c,'native'),
                   y=unit(0,'native'),
                   width=unit(r.w,'native'),
                   height=unit(180,'native'))  
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
Draw.temperature<-function(temperature,Options,Trange=1) {

  Options.local<-Options
  Options.local$fog.min.transparency<-0.8
  Options.local$fog.resolution<-0.25
  tplus<-temperature
  tplus$data[]<-pmax(0,pmin(Trange,tplus$data))
  tplus$data[]<-sqrt(tplus$data[])/Trange
  Options.local$fog.colour<-c(1,0,0)
  WeatherMap.draw.fog(tplus,Options.local)
  tminus<-temperature
  tminus$data[]<-tminus$data*-1
  tminus$data[]<-pmax(0,pmin(Trange,tminus$data))
  tminus$data[]<-sqrt(tminus$data[])/Trange
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

    sf.name<-sprintf("%s/streamlines.%04d-%02d-%02d:%02d:%02d.rd",
                           Stream.dir,year,month,day,as.integer(hour),as.integer((hour%%1)*60))
    if(file.exists(sf.name) && file.info(sf.name)$size>5000) {
       load(sf.name)
      return(s)
    } else {
      stop(sprintf("No streamlines available for %04d-%02d-%02d:%02d",
                   year,month,day,as.integer(hour)))
    }

}

plot.hour<-function(year,month,day,hour,streamlines) {

    image.name<-sprintf("%04d-%02d-%02d:%02d.%02d.png",year,month,day,as.integer(hour),
                        as.integer((hour%%1)*100))

    ifile.name<-sprintf("%s/%s",Imagedir,image.name)
    if(file.exists(ifile.name) && file.info(ifile.name)$size>0) return()

    land<-WeatherMap.get.land(Options)
    
    t2m<-ERAI.get.slice.at.hour('air.2m',year,month,day,hour)
    hour.fraction<-hour%%1
    t2n<-ERAI.get.slice.at.hour('air.2m',year,month,day,hour,type='normal')
    t2m$data[]<-t2m$data-t2n$data
    prmsl.T<-ERAI.get.slice.at.hour('prmsl',year,month,day,hour)
    icec<-ERAI.get.slice.at.hour('icec',year,month,day,hour)
    prate<-ERAI.get.slice.at.hour('prate',year,month,day,hour,fc.init='blend')
    prate$data[]<-prate$data/3
  
     png(ifile.name,
             width=1080*4/3,
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
      WeatherMap.draw.ice(ip$lat,ip$lon,icec,Options)
      draw.land.flat(Options)
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

