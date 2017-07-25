#!/usr/bin/env Rscript 

# Pressure spaghetti plot for V3 compared with V2c
# Render just one timestep - parallelise on SPICE.

library(GSDF.TWCR)
library(GSDF.WeatherMap)
library(grid)
library(getopt)

opt = getopt(matrix(c(
  'year',   'y', 2, "integer",
  'month',  'm', 2, "integer",
  'day',    'd', 2, "integer",
  'hour',   'h', 2, "numeric"
), byrow=TRUE, ncol=4))
if ( is.null(opt$year) )   { stop("Year not specified") }
if ( is.null(opt$month) )  { stop("Month not specified") }
if ( is.null(opt$day) )    { stop("Day not specified") }
if ( is.null(opt$hour) )   { stop("Hour not specified") }

Imagedir<-sprintf("%s/images/TWCR_spaghetti.V3vV2c.anomalies",Sys.getenv('SCRATCH'))
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
range<-85
aspect<-8/9
Options<-WeatherMap.set.option(Options,'lat.min',range*-1)
Options<-WeatherMap.set.option(Options,'lat.max',range)
Options<-WeatherMap.set.option(Options,'lon.min',range*aspect*-1)
Options<-WeatherMap.set.option(Options,'lon.max',range*aspect)
Options<-WeatherMap.set.option(Options,'pole.lon',173)
Options<-WeatherMap.set.option(Options,'pole.lat',36)

Options$mslp.base=0                    # Base value for anomalies
Options$mslp.range=50000                    # Anomaly for max contour
Options$mslp.step=750                       # Smaller -> more contours
Options$mslp.tpscale=500                    # Smaller -> contours less transparent
Options$mslp.lwd=0.75
Options$label.xp=0.995

Draw.pressure<-function(mslp,Options,colour=c(0,0,0,1)) {
  
 M<-GSDF.WeatherMap:::WeatherMap.rotate.pole(mslp,Options)
  M<-GSDF:::GSDF.pad.longitude(M) # Extras for periodic boundary conditions
  lats<-M$dimensions[[GSDF.find.dimension(M,'lat')]]$values
  longs<-M$dimensions[[GSDF.find.dimension(M,'lon')]]$values
    # Need particular data format for contourLines
  maxl<-Options$lon.max+(lats[2]-lats[1])
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
       plot.hour.V3(year,month,day,hour)

    popViewport()
    
     pushViewport(viewport(x=unit(0.25,'npc'),y=unit(0.5,'npc'),
                           width=unit(0.5,'npc'),height=unit(1,'npc'),
                           clip='on'))
        
       grid.polygon(x=unit(c(0,1,1,0),'npc'),
                    y=unit(c(0,0,1,1),'npc'),
                    gp=gpar(fill=Options$sea.colour))
       plot.hour.V2c(year,month,day,hour)

    popViewport()

    grid.lines(x=unit(c(0.5,0.5),'npc'),
               y=unit(c(0,1),'npc'),
               gp=gpar(col=rgb(1,1,0.5),lwd=2))
    
    dev.off()

}

plot.hour.V3<-function(year,month,day,hour) {

   
    pre<-TWCR.get.members.slice.at.hour('prmsl',year,month,day,
                                  hour,version='4.1.8')
    pn<-TWCR.get.slice.at.hour('prmsl',year,month,day,hour,version='4.0.0',type='normal')
    obs<-TWCR.get.obs(year,month,day,hour,version='4.1.8')
    w<-which(obs$Longitude>180)
    obs$Longitude[w]<-obs$Longitude[w]-360
  
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
    
      WeatherMap.draw.land(land,Options)
      WeatherMap.draw.obs(obs,Options)
      for(vn in seq(1,80)) {
            m<-GSDF.select.from.1d(pre,'ensemble',vn)
            if(vn==1) pn<-GSDF.regrid.2d(pn,m)
            m$data[]<-as.vector(m$data)-as.vector(pn$data)
            try(Draw.pressure(m,Options,colour=c(0,0,0,0.25)))
          }
     
      Options$label=sprintf("%04d-%02d-%02d:%02d",year,month,day,as.integer(hour))
    WeatherMap.draw.label(Options)
    popViewport()
}

plot.hour.V2c<-function(year,month,day,hour) {

   
    pre<-TWCR.get.members.slice.at.hour('prmsl',year,month,day,
                                  hour,version='3.5.1')
    pn<-TWCR.get.slice.at.hour('prmsl',year,month,day,hour,version='3.4.1',type='normal')
    obs<-TWCR.get.obs(year,month,day,hour,version='3.5.1')
    w<-which(obs$Longitude>180)
    obs$Longitude[w]<-obs$Longitude[w]-360
  
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
    
      WeatherMap.draw.land(land,Options)
      WeatherMap.draw.obs(obs,Options)
      for(vn in seq(1,56)) {
            m<-GSDF.select.from.1d(pre,'ensemble',vn)
            if(vn==1) pn<-GSDF.regrid.2d(pn,m)
            m$data[]<-as.vector(m$data)-as.vector(pn$data)
            try(Draw.pressure(m,Options,colour=c(0,0,0,0.25)))
          }
     
      Options$label=sprintf("%04d-%02d-%02d:%02d",year,month,day,as.integer(hour))
    popViewport()
}

land<-WeatherMap.get.land(Options)
plot.hour(opt$year,opt$month,opt$day,opt$hour)
#warnings()
