#!/usr/bin/env Rscript 

# Wind, ice, pressure, temperature and precip polyhedra.
# Just do the rendering - use pre-calculated streamlines
# Render just one timestep - parallelise on SPICE.
# Sub-hourly version - fudge streamlines

library(GSDF.TWCR)
library(GSDF.WeatherMap)
library(grid)
library(getopt)

opt = getopt(matrix(c(
  'year',   'y', 2, "integer",
  'month',  'm', 2, "integer",
  'day',    'd', 2, "integer",
  'hour',   'h', 2, "numeric",
  'version','v', 2, "character"
), byrow=TRUE, ncol=4))
if ( is.null(opt$year) )   { stop("Year not specified") }
if ( is.null(opt$month) )  { stop("Month not specified") }
if ( is.null(opt$day) )    { stop("Day not specified") }
if ( is.null(opt$hour) )   { stop("Hour not specified") }
if ( is.null(opt$version) ){ opt$version='4.1.8' }
member=1
fog.threshold<-exp(1)

Imagedir<-sprintf("%s/images/TWCR_multivariate.mean.V3vV2c.nf",Sys.getenv('SCRATCH'))
Stream.dir.V3<-sprintf("%s/images/TWCR_multivariate.mean.V3",Sys.getenv('SCRATCH'))
Stream.dir.V2c<-sprintf("%s/images/TWCR_multivariate.mean.V2c",Sys.getenv('SCRATCH'))
if(!file.exists(Imagedir)) dir.create(Imagedir,recursive=TRUE)

Options<-WeatherMap.set.option(NULL)
Options<-WeatherMap.set.option(Options,'land.colour',rgb(100,100,100,255,
                                                       maxColorValue=255))
Options<-WeatherMap.set.option(Options,'sea.colour',rgb(150,150,150,255,
                                                       maxColorValue=255))
Options<-WeatherMap.set.option(Options,'ice.colour',rgb(250,250,250,255,
                                                       maxColorValue=255))
range<-85
aspect<-8/9
Options<-WeatherMap.set.option(Options,'lat.min',range*-1)
Options<-WeatherMap.set.option(Options,'lat.max',range)
Options<-WeatherMap.set.option(Options,'lon.min',range*aspect*-1)
Options<-WeatherMap.set.option(Options,'lon.max',range*aspect)
Options<-WeatherMap.set.option(Options,'pole.lon',173)
Options<-WeatherMap.set.option(Options,'pole.lat',36)

Options$mslp.base=0#101325                    # Base value for anomalies
Options$mslp.range=50000                    # Anomaly for max contour
Options$mslp.step=500                       # Smaller -> more contours
Options$mslp.tpscale=500                    # Smaller -> contours less transparent
Options$mslp.lwd=1
Options$precip.colour=c(0,0.2,0)
Options$label.xp=0.995

get.mean.at.hour<-function(variable,year,month,day,hour,version='4.1.8') {

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
  maxl<-Options$lon.max+(longs[2]-longs[1])
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

get.streamlines<-function(year,month,day,hour,dir) {


    sf.name<-sprintf("%s/streamlines.%04d-%02d-%02d:%02d.rd",
                           dir,year,month,day,as.integer(hour))
    if(file.exists(sf.name) && file.info(sf.name)$size>5000) {
       load(sf.name)
       hour.fraction<-hour-as.integer(hour)
       # Fudge the streamlines for the fractional hour
       if(hour.fraction>0) {
          move.scale<-0.033*Options$wind.vector.points/Options$wind.vector.scale
          move.scale<-move.scale*Options$wind.vector.move.scale*view.scale
          for(p in seq(1,Options$wind.vector.points)) {
           s[['x']][,p]<-s[['x']][,p]+(s[['x']][,2]-s[['x']][,1])*move.scale*hour.fraction
           s[['y']][,p]<-s[['y']][,p]+(s[['y']][,2]-s[['y']][,1])*move.scale*hour.fraction
         }
       }
       return(s)
    } else {
      stop(sprintf("No streamlines available for %04d-%02d-%02d:%02d",
                   year,month,day,as.integer(hour)))
    }

}

plot.hour<-function(year,month,day,hour) {
  
    image.name<-sprintf("%04d-%02d-%02d:%02d.%02d.png",year,month,day,as.integer(hour),
                        as.integer((hour%%1)*100))

    ifile.name<-sprintf("%s/%s",Imagedir,image.name)
    if(file.exists(ifile.name) && file.info(ifile.name)$size>0) return()
 
     png(ifile.name,
             width=1080*16/9,
             height=1080,
             bg='white',
             pointsize=24,
             type='cairo-png')
     base.gp<-gpar(family='Helvetica',font=1,col='black')

     pushViewport(viewport(x=unit(0.75,'npc'),y=unit(0.5,'npc'),
                           width=unit(0.5,'npc'),height=unit(1,'npc'),
                           clip='on'))
        
       grid.polygon(x=unit(c(0,1,1,0),'npc'),
                    y=unit(c(0,0,1,1),'npc'),
                    gp=gpar(fill=Options$sea.colour))
       s<-get.streamlines(opt$year,opt$month,opt$day,opt$hour,Stream.dir.V3)
       plot.hour.V3(year,month,day,hour,s)

    popViewport()
    
     pushViewport(viewport(x=unit(0.25,'npc'),y=unit(0.5,'npc'),
                           width=unit(0.5,'npc'),height=unit(1,'npc'),
                           clip='on'))
        
       grid.polygon(x=unit(c(0,1,1,0),'npc'),
                    y=unit(c(0,0,1,1),'npc'),
                    gp=gpar(fill=Options$sea.colour))
       s<-get.streamlines(opt$year,opt$month,opt$day,opt$hour,Stream.dir.V2c)
       plot.hour.V2c(year,month,day,hour,s)

    popViewport()

    grid.lines(x=unit(c(0.5,0.5),'npc'),
               y=unit(c(0,1),'npc'),
               gp=gpar(col=rgb(1,1,0.5),lwd=2))
    
    dev.off()

}

plot.hour.V3<-function(year,month,day,hour,streamlines) {
   
    t2m<-get.mean.at.hour('air.2m',year,month,day,hour,version=opt$version)
    t2n<-TWCR.get.slice.at.hour('air.2m',year,month,day,hour,version='4.0.0',type='normal')
    t2n<-GSDF.regrid.2d(t2n,t2m)
    t2m$data[]<-as.vector(t2m$data)-as.vector(t2n$data)
    
    pre<-get.mean.at.hour('prmsl',year,month,day,hour,version=opt$version)
    prn<-TWCR.get.slice.at.hour('prmsl',year,month,day,hour,version='4.0.0',type='normal')
    prn<-GSDF.regrid.2d(prn,pre)
    pre$data[]<-as.vector(pre$data)-as.vector(prn$data)
   
    icec<-get.mean.at.hour('icec',year,month,day,hour,version=opt$version)
    prate<-get.mean.at.hour('prate',year,month,day,hour,version=opt$version)
  
  lon.min<-Options$lon.min
  if(!is.null(Options$vp.lon.min)) lon.min<-Options$vp.lon.min
  lon.max<-Options$lon.max
  if(!is.null(Options$vp.lon.max)) lon.max<-Options$vp.lon.max
  lat.min<-Options$lat.min
  if(!is.null(Options$vp.lat.min)) lat.min<-Options$vp.lat.min
  lat.max<-Options$lat.max
  if(!is.null(Options$vp.lat.max)) lat.max<-Options$vp.lat.max
  pushViewport(dataViewport(c(lon.min,lon.max),c(lat.min,lat.max),
		            extension=0))
    
      ip<-WeatherMap.rectpoints(Options$ice.points,Options)
      WeatherMap.draw.ice(ip$lat,ip$lon,icec,Options)
      WeatherMap.draw.land(land,Options)
      WeatherMap.draw.streamlines(streamlines,Options)
       Draw.temperature(t2m,Options,Trange=10)
       WeatherMap.draw.precipitation(prate,Options)
       Draw.pressure(pre,Options,colour=c(0,0,0))
    Options$label=sprintf("%04d-%02d-%02d:%02d",year,month,day,as.integer(hour))
    WeatherMap.draw.label(Options)
  popViewport()
}

plot.hour.V2c<-function(year,month,day,hour,streamlines) {
   
    t2m<-get.mean.at.hour('air.2m',year,month,day,hour,version='3.5.1')
    t2n<-TWCR.get.slice.at.hour('air.2m',year,month,day,hour,version='3.4.1',type='normal')
    t2n<-GSDF.regrid.2d(t2n,t2m)
    t2m$data[]<-as.vector(t2m$data)-as.vector(t2n$data)
    
    pre<-get.mean.at.hour('prmsl',year,month,day,hour,version='3.5.1')
    prn<-TWCR.get.slice.at.hour('prmsl',year,month,day,hour,version='3.4.1',type='normal')
    prn<-GSDF.regrid.2d(prn,pre)
    pre$data[]<-as.vector(pre$data)-as.vector(prn$data)
   
    icec<-TWCR.get.slice.at.hour('icec',year,month,day,hour,version='3.5.1')
    prate<-get.mean.at.hour('prate',year,month,day,hour,version='3.5.1')
  
  lon.min<-Options$lon.min
  if(!is.null(Options$vp.lon.min)) lon.min<-Options$vp.lon.min
  lon.max<-Options$lon.max
  if(!is.null(Options$vp.lon.max)) lon.max<-Options$vp.lon.max
  lat.min<-Options$lat.min
  if(!is.null(Options$vp.lat.min)) lat.min<-Options$vp.lat.min
  lat.max<-Options$lat.max
  if(!is.null(Options$vp.lat.max)) lat.max<-Options$vp.lat.max
  pushViewport(dataViewport(c(lon.min,lon.max),c(lat.min,lat.max),
		            extension=0))
    
      ip<-WeatherMap.rectpoints(Options$ice.points,Options)
      WeatherMap.draw.ice(ip$lat,ip$lon,icec,Options)
      WeatherMap.draw.land(land,Options)
      WeatherMap.draw.streamlines(streamlines,Options)
       Draw.temperature(t2m,Options,Trange=10)
       WeatherMap.draw.precipitation(prate,Options)
       Draw.pressure(pre,Options,colour=c(0,0,0))
  popViewport()
}

land<-WeatherMap.get.land(Options)
plot.hour(opt$year,opt$month,opt$day,opt$hour)

