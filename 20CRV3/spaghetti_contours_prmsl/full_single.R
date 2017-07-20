#!/usr/bin/env Rscript 

# Pressure spaghetti plot for V3
# Render just one timestep - parallelise on SPICE.

library(GSDF.TWCR)
library(GSDF.WeatherMap)
library(grid)
library(getopt)

opt = getopt(c(
  'year',   'y', 2, "integer",
  'month',  'm', 2, "integer",
  'day',    'd', 2, "integer",
  'hour',   'h', 2, "numeric",
  'version','v', 2, "character"
))
if ( is.null(opt$year) )   { stop("Year not specified") }
if ( is.null(opt$month) )  { stop("Month not specified") }
if ( is.null(opt$day) )    { stop("Day not specified") }
if ( is.null(opt$hour) )   { stop("Hour not specified") }
if ( is.null(opt$version) ){ opt$version='4.1.8' }
members<-seq(1,80)

Imagedir<-sprintf("%s/images/TWCR_spaghetti.V3",Sys.getenv('SCRATCH'))
if(!file.exists(Imagedir)) dir.create(Imagedir,recursive=TRUE)

Options<-WeatherMap.set.option(NULL)
Options<-WeatherMap.set.option(Options,'land.colour',rgb(100,100,100,255,
                                                       maxColorValue=255))
Options<-WeatherMap.set.option(Options,'sea.colour',rgb(150,150,150,255,
                                                       maxColorValue=255))
Options<-WeatherMap.set.option(Options,'ice.colour',rgb(250,250,250,255,
                                                       maxColorValue=255))
Options<-WeatherMap.set.option(Options,'obs.size',0.75)
Options<-WeatherMap.set.option(Options,'obs.colour',rgb(0,0,0,255,
                                                       maxColorValue=255))
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

Options$mslp.base=0#101325                    # Base value for anomalies
Options$mslp.range=50000                    # Anomaly for max contour
Options$mslp.step=750                       # Smaller -> more contours
Options$mslp.tpscale=500                    # Smaller -> contours less transparent
Options$mslp.lwd=0.75
Options$label.xp=0.995

# Fudge for using the V2 climatology
clim.correct<-list()
clim.correct[['air.2m']]<-readRDS('/scratch/hadpb/20CR/version_4.0.0/air.2m.normals.correction.Rdata')
clim.correct[['prmsl']]<-readRDS('/scratch/hadpb/20CR/version_4.0.0/prmsl.normals.correction.Rdata')

get.V3.normal<-function(variable,year,month,day,hour) {
  n<-TWCR.get.slice.at.hour(variable,year,month,day,hour,type='normal',version='3.4.1')
  adj<-clim.correct[[variable]][['00']]
  n<-GSDF.regrid.2d(n,adj)
  if(hour==0) {
    n$data[]<-n$data+as.vector(clim.correct[[variable]][['00']]$data)
    return(n)
  }
  if(hour<6) {
    weight<-(hour)/6
    n$data[]<-n$data +
              as.vector(clim.correct[[variable]][['06']]$data)*weight +
              as.vector(clim.correct[[variable]][['00']]$data)*(1-weight)
    return(n)
  }
  if(hour==6) {
    n$data[]<-n$data+as.vector(clim.correct[[variable]][['06']]$data)
    return(n)
  }
  if(hour<12) {
    weight<-(hour-6)/6
    n$data[]<-n$data +
              as.vector(clim.correct[[variable]][['12']]$data)*weight +
              as.vector(clim.correct[[variable]][['06']]$data)*(1-weight)
    return(n)
  }
  if(hour==12) {
    n$data[]<-n$data+as.vector(clim.correct[[variable]][['12']]$data)
    return(n)
  }
  if(hour<18) {
    weight<-(hour-12)/6
    n$data[]<-n$data +
              as.vector(clim.correct[[variable]][['18']]$data)*weight +
              as.vector(clim.correct[[variable]][['12']]$data)*(1-weight)
    return(n)
  }
  if(hour==18) {
    n$data[]<-n$data+as.vector(clim.correct[[variable]][['18']]$data)
    return(n)
  }
  weight<-(hour-18)/6
  n$data[]<-n$data +
            as.vector(clim.correct[[variable]][['00']]$data)*weight +
            as.vector(clim.correct[[variable]][['18']]$data)*(1-weight)
  return(n)
}

Draw.pressure<-function(mslp,Options,colour=c(0,0,0,1)) {
  
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
           lt<-1
           lwd<-1
           gp<-gpar(col=rgb(0.5,0,0,tp*colour[4]),
                               lwd=Options$mslp.lwd*lwd,lty=lt)
           if(lines[[i]]$level<=Options$mslp.base) {
               lt<-1
               lwd<-1
               gp<-gpar(col=rgb(0,0,0.5,tp*colour[4]),
                               lwd=Options$mslp.lwd*lwd,lty=lt)
           }
             
           grid.xspline(x=unit(lines[[i]]$x,'native'),
                      y=unit(lines[[i]]$y,'native'),
                      shape=1,
                      gp=gp)
       }
    }
  }

plot.hour<-function(year,month,day,hour) {

    image.name<-sprintf("%04d-%02d-%02d:%02d.%02d.png",year,month,day,as.integer(hour),
                        as.integer((hour%%1)*100))

    ifile.name<-sprintf("%s/%s",Imagedir,image.name)
    if(file.exists(ifile.name) && file.info(ifile.name)$size>0) return()

    land<-WeatherMap.get.land(Options)
    
    pre<-TWCR.get.members.slice.at.hour('prmsl',year,month,day,
                                  hour,version=opt$version)
    pn<-get.V3.normal('prmsl',year,month,day,hour)
    pn<-GSDF.regrid.2d(pn,GSDF.select.from.1d(pre,'ensemble',1))
    obs<-TWCR.get.obs(year,month,day,hour,version=opt$version)
    w<-which(obs$Longitude>180)
    obs$Longitude[w]<-obs$Longitude[w]-360
 
     png(ifile.name,
             width=1080*16/9,
             height=1080,
             bg=Options$sea.colour,
             pointsize=24,
             type='cairo-png')
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
      WeatherMap.draw.obs(obs,Options)
      for(vn in seq_along(members)) {
            m<-GSDF.select.from.1d(pre,'ensemble',vn)
            m$data[]<-m$data-as.vector(pn$data)
  	    Draw.pressure(m,Options,colour=c(0,0,0,0.25))
          }
     
      Options$label=sprintf("%04d-%02d-%02d:%02d",year,month,day,as.integer(hour))
    WeatherMap.draw.label(Options)
    dev.off()
}

plot.hour(opt$year,opt$month,opt$day,opt$hour)
#warnings()
