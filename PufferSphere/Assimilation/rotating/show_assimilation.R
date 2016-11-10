#!/usr/bin/Rscript

# Show the process of assimilation in 20CR

library(GSDF.TWCR)
library(GSDF.WeatherMap)
library(lubridate)
library(getopt)
library(RColorBrewer)

opt = getopt(c(
  'year',   'y', 2, "integer",
  'month',  'm', 2, "integer",
  'day',    'd', 2, "integer",
  'step',   's', 2, "integer"   # 1-28
));
if ( is.null(opt$year  ) )   { stop("Year not specified") }
if ( is.null(opt$month  ) )  { stop("Month not specified") }
if ( is.null(opt$day  ) )    { stop("Day not specified") }
if ( is.null(opt$step  ) )   { stop("Step not specified") }

Year<-opt$year
Month<-opt$month
Day<-opt$day
version<-'3.5.1'
members<-seq(1,56)

GSDF.cache.dir<-sprintf("%s/GSDF.cache",Sys.getenv('SCRATCH'))
if(!file.exists(GSDF.cache.dir)) dir.create(GSDF.cache.dir,recursive=TRUE)
Imagedir<-sprintf("%s/images/1998-assimilation-rotating",Sys.getenv('SCRATCH'),version)
if(!file.exists(Imagedir)) dir.create(Imagedir,recursive=TRUE)


Options<-WeatherMap.set.option(NULL)
Options<-WeatherMap.set.option(Options,'show.mslp',T)
Options<-WeatherMap.set.option(Options,'show.ice',F)
Options<-WeatherMap.set.option(Options,'show.obs',T)
Options<-WeatherMap.set.option(Options,'show.fog',F)
Options<-WeatherMap.set.option(Options,'show.precipitation',F)
Options<-WeatherMap.set.option(Options,'temperature.range',6)

Options<-WeatherMap.set.option(Options,'lat.min',-90)
Options<-WeatherMap.set.option(Options,'lat.max',90)
Options<-WeatherMap.set.option(Options,'lon.min',-190)
Options<-WeatherMap.set.option(Options,'lon.max',190)
Options$vp.lon.min<- -180
Options$vp.lon.max<-  180
Options<-WeatherMap.set.option(Options,'pole.lon',160)
Options<-WeatherMap.set.option(Options,'pole.lat',35)
Options<-WeatherMap.set.option(Options,'background.resolution','high')
Options<-WeatherMap.set.option(Options,'land.colour',rgb(0,0,0,255,
                                                       maxColorValue=255))
Options<-WeatherMap.set.option(Options,'sea.colour',rgb(100,100,100,255,
                                                       maxColorValue=255))
Options$obs.size<- 0.75

Options$mslp.lwd<-1
Options$mslp.base=0                         # Base value for anomalies
Options$mslp.range=50000                    # Anomaly for max contour
Options$mslp.crange=3000                    # Anomaly for max contour colour
Options$mslp.step=1000                      # Smaller -more contours
Options$mslp.tpscale=350                    # Smaller -contours less transparent
# Overrides mslp options options
contour.levels<-seq(-300,300,30)
contour.levels<-abs(contour.levels)**1.5*sign(contour.levels)

cols.base<-brewer.pal(11,"RdBu")
cols.base<-c(cols.base[1:3],"#A0A0A0",cols.base[9:11]) # Filter out the white bits
cols<-colorRampPalette(cols.base)(100)

sigmoid<-function(x) {
  x<-x/1000 # want range about -5<->5, scale for hPa anomalies
  s<-as.integer(100*(1 / (1 + exp(x))))+1
  s<-max(1,min(s,100))
  return(s)
}
set.temperature.colour<-function(data) {
    return(cols[sigmoid(data)])
}
set.pole<-function(year,month,day,hour,Options,d.step) {
  step<-as.integer(yday(ymd(sprintf("%04d-%02d-%02d",year,month,day)))*184+d.step)
  if(step<=1000) return(Options)
  lon<-160+((step-1000)/10)
  if(lon>360) lon<-lon%%360
  lat<-35+sin((step-1000)/500)*20
  Options<-WeatherMap.set.option(Options,'pole.lon',lon)
  Options<-WeatherMap.set.option(Options,'pole.lat',lat)
  min.lon<-((step-1000)/5)%%360-180
  Options<-WeatherMap.set.option(Options,'lon.min',min.lon-10)
  Options<-WeatherMap.set.option(Options,'lon.max',min.lon+380)
  Options<-WeatherMap.set.option(Options,'vp.lon.min',min.lon   )
  Options<-WeatherMap.set.option(Options,'vp.lon.max',min.lon+360)
  return(Options)
}

# Estimate a first-guess ensemble by scaling the analysis
#  ensemble to have the first-guess mean and spread
inflate.ensemble<-function(ensemble,fg.mean,fg.spread) {
  ens.mean<-GSDF.reduce.1d(ensemble,'ensemble',mean)
  ens.sd<-GSDF.reduce.1d(ensemble,'ensemble',sd)
  fg.mean<-GSDF.regrid.2d(fg.mean,ens.mean)
  fg.spread<-GSDF.regrid.2d(fg.spread,ens.mean)
  for(i in seq(1,length(ensemble$dimensions[[1]]$values))) {
     for(j in seq(1,length(ensemble$dimensions[[2]]$values))) {
        ensemble$data[i,j,,1]<-(ensemble$data[i,j,,1]-ens.mean$data[i,j,1])*
                                 fg.spread$data[i,j,1]/ens.sd$data[i,j,1]+
                                 fg.mean$data[i,j,1]
      }
  }
  return(ensemble)
}

# Special functions for getting data
get.forecast.step.start<-function(year,month,day,hour) {
  hour<-as.integer(hour)
  hour<-hour-hour%%6
  e<-TWCR.get.members.slice.at.hour('prmsl',year,month,day,
                                    hour,version=version)
  return(e)
}
get.forecast.step.end<-function(year,month,day,hour) {
  hour<-as.integer(hour)
  hour<-hour-hour%%6+6
  if(hour>23) {
    ymd<-ymd(sprintf("%04d-%02d-%02d",year,month,day))+days(1)
    year<-year(ymd)
    month<-month(ymd)
    day<-day(ymd)
    hour<-hour-24
  }
  e<-TWCR.get.members.slice.at.hour('prmsl',year,month,day,
                                    hour,version=version)
  fg.m<-TWCR.get.slice.at.hour('prmsl',year,month,day,hour,
                                type='fg.mean',version=version)
  fg.s<-TWCR.get.slice.at.hour('prmsl',year,month,day,hour,
                                type='fg.spread',version=version)
  e<-inflate.ensemble(e,fg.m,fg.s)
  return(e)
}
get.forecast.step.interpolated<-function(year,month,day,hour) {
  e1<-get.forecast.step.start(year,month,day,hour)
  e2<-get.forecast.step.end(year,month,day,hour)
  stage<-(hour%%6)/6
  e1$data[]<-e2$data*stage+e1$data*(1-stage)
  return(e1)
}
get.assimilation.step.start<-function(year,month,day,hour) {
  return(get.forecast.step.end(year,month,day,hour-1))
}
get.assimilation.step.end<-function(year,month,day,hour) {
  return(get.forecast.step.start(year,month,day,hour+1))
}
get.assimilation.step.interpolated<-function(year,month,day,hour,stage) {
  e1<-get.assimilation.step.start(year,month,day,hour)
  e2<-get.assimilation.step.end(year,month,day,hour)
  e1$data[]<-e2$data*stage+e1$data*(1-stage)
  return(e1)
}
 
obs.get.colour<-function(mp) {
   if(mp>0) return(rgb(1,0,0))
   return(rgb(0,0,1))
}

Draw.obs.pressure<-function(obs,Options) {

  
  w<-which(obs$SLP>0)
  if(length(w)>0) {
    Options$obs.colour<-obs.get.colour(10)
    WeatherMap.draw.obs(obs[w,],Options)
  }
  if(length(w)<length(obs$SLP)) {
    Options$obs.colour<-obs.get.colour(-10)
    WeatherMap.draw.obs(obs[-w,],Options)
  }
}

Draw.obs.pressure<-function(obs,Options) {

  min.pressure<-Options$mslp.base-Options$mslp.crange
  w<-which(obs$SLP<min.pressure)
  if(length(w)>0) {
    Options$obs.colour<-set.temperature.colour(min.pressure)
    WeatherMap.draw.obs(obs[w,],Options)
  }
  for(mp in seq(Options$mslp.base-Options$mslp.crange+Options$mslp.step,
                Options$mslp.base+Options$mslp.crange,Options$mslp.step)) {
    w<-which(obs$SLP<mp & obs$SLP>=mp-Options$mslp.step)
    if(length(w)>0) {
      Options$obs.colour<-set.temperature.colour(mp-Options$mslp.step/2)
      WeatherMap.draw.obs(obs[w,],Options)
    }
 }
  max.pressure<-Options$mslp.base+Options$mslp.crange
  w<-which(obs$SLP>max.pressure)
  if(length(w)>0) {
    Options$obs.colour<-set.temperature.colour(max.pressure)
    WeatherMap.draw.obs(obs[w,],Options)
  }
}


Draw.pressure<-function(mslp,Options,colour=c(0,0,0,1)) {
  
    M<-GSDF.WeatherMap:::WeatherMap.rotate.pole(mslp,Options)
    lats<-M$dimensions[[GSDF.find.dimension(M,'lat')]]$values
    longs<-M$dimensions[[GSDF.find.dimension(M,'lon')]]$values
      # Need particular data format for contourLines
    maxl<-Options$vp.lon.max+2
    if(lats[2]<lats[1] || longs[2]<longs[1] || max(longs)> maxl ) {
      if(lats[2]<lats[1]) lats<-rev(lats)
      if(longs[2]<longs[1]) longs<-rev(longs)
      longs[longs>maxl]<-longs[longs>maxl]-(maxl*2)
      longs<-sort(longs)
      M2<-M
      M2$dimensions[[GSDF.find.dimension(M,'lat')]]$values<-lats
      M2$dimensions[[GSDF.find.dimension(M,'lon')]]$values<-longs
      M<-GSDF.regrid.2d(M,M2)
      M<-GSDF:::GSDF.pad.longitude(M)
    } else {
       M<-GSDF:::GSDF.pad.longitude(M)
     }
    lats<-M$dimensions[[GSDF.find.dimension(M,'lat')]]$values
    longs<-M$dimensions[[GSDF.find.dimension(M,'lon')]]$values
    z<-matrix(data=M$data,nrow=length(longs),ncol=length(lats))
    lines<-contourLines(longs,lats,z,
                         levels=contour.levels)
    if(!is.na(lines) && length(lines)>0) {
       for(i in seq(1,length(lines))) {
           gp<-gpar(col=set.temperature.colour(lines[[i]]$level),alpha=0.5,
                    lwd=Options$mslp.lwd,lty=1)
             
           grid.xspline(x=unit(lines[[i]]$x,'native'),
                      y=unit(lines[[i]]$y,'native'),
                      shape=1,
                      gp=gp)
       }
    }
  }

plot.forecast.hour<-function(year,month,day,hour,d.step) {    

    image.name<-sprintf("%04d-%02d-%02d:%02d:%02d.99.png",year,month,day,as.integer(hour),
                                                         as.integer(hour%%1*60))
    ifile.name<-sprintf("%s/%s",Imagedir,image.name)
    if(file.exists(ifile.name) && file.info(ifile.name)$size>0) return()

    Options<-set.pole(year,month,day,hour,Options,d.step)
    land<-WeatherMap.get.land(Options)

    png(ifile.name,
             width=1080*2,
             height=1080,
             bg=Options$sea.colour,
             pointsize=24,
             type='cairo')
    Options$label<-sprintf("%04d-%02d-%02d:%02d:%02d",year,month,day,as.integer(hour),
                                                         as.integer(hour%%1*60))
  
  	   pushViewport(dataViewport(c(Options$vp.lon.min,Options$vp.lon.max),
  				     c(Options$lat.min,Options$lat.max),
  				      extension=0))
  	      WeatherMap.draw.land(land,Options)
      prmsl.normal<-TWCR.get.slice.at.hour('prmsl',year,month,day,hour,version='3.4.1',
                                               type='normal')
      e<-get.forecast.step.interpolated(year,month,day,
                                  hour)
      m<-GSDF.select.from.1d(e,'ensemble',1)
      prmsl.normal<-GSDF.regrid.2d(prmsl.normal,m)
      for(vn in seq_along(members)) {
            m<-GSDF.select.from.1d(e,'ensemble',vn)
  	    m$data[]<-as.vector(m$data)-as.vector(prmsl.normal$data)
  	    Draw.pressure(m,Options,colour=c(0,0,0,0.5))
          }
      #WeatherMap.draw.label(Options)
      gc()
    dev.off()
}

plot.assimilation.stage<-function(year,month,day,hour,stage,d.step) {    

    image.name<-sprintf("%04d-%02d-%02d:%02d:%02d.%02d.png",year,month,day,as.integer(hour),
                                                         as.integer(hour%%1*60),as.integer(stage*100)+1)
    ifile.name<-sprintf("%s/%s",Imagedir,image.name)
    if(file.exists(ifile.name) && file.info(ifile.name)$size>0) return()

    Options<-set.pole(year,month,day,hour,Options,d.step)
    land<-WeatherMap.get.land(Options)

     png(ifile.name,
             width=1080*2,
             height=1080,
             bg=Options$sea.colour,
             pointsize=24,
             type='cairo')
    Options$label<-sprintf("%04d-%02d-%02d:%02d:%02d",year,month,day,as.integer(hour),
                                                         as.integer(hour%%1*60))
  
  	   pushViewport(dataViewport(c(Options$vp.lon.min,Options$vp.lon.max),
  				     c(Options$lat.min,Options$lat.max),
  				      extension=0))
  	      WeatherMap.draw.land(land,Options)
     
       prmsl.normal<-TWCR.get.slice.at.hour('prmsl',year,month,day,hour,version='3.4.1',
                                               type='normal')
      e<-get.assimilation.step.interpolated(year,month,day,
                                  hour,stage)
      m<-GSDF.select.from.1d(e,'ensemble',1)
      prmsl.normal<-GSDF.regrid.2d(prmsl.normal,m)
      obs<-TWCR.get.obs(year,month,day,hour,version=version,range=0.15)
      w<-which(obs$Longitude>180)
      obs$Longitude[w]<-obs$Longitude[w]-360
      fg.m<-TWCR.get.slice.at.hour('prmsl',year,month,day,hour,
                                   type='fg.mean',version=version)    
      obs$SLP<-obs$Mean.first.guess.pressure.difference*100+
               GSDF.interpolate.ll(fg.m,obs$Latitude,obs$Longitude)-
               GSDF.interpolate.ll(prmsl.normal,obs$Latitude,obs$Longitude)
      Draw.obs.pressure(obs,Options)
      for(vn in seq_along(members)) {
            m<-GSDF.select.from.1d(e,'ensemble',vn)
  	    m$data[]<-as.vector(m$data)-as.vector(prmsl.normal$data)
  	    Draw.pressure(m,Options,colour=c(0,0,0,0.5))
          }
      #WeatherMap.draw.label(Options)
      gc()
    dev.off()
}

for(hour in c(0,6,12,18)) {
  d.step<-hour*6+as.integer(hour/6)*10
  for(stage in seq(0.05,0.95,0.1)) {
    d.step<-d.step+1
    if(opt$step==hour/6+1) plot.assimilation.stage(Year,Month,Day,hour,stage,d.step)
  }
}

for(hour in seq(0,23)) {
  d.step<-hour*6+as.integer(hour/6)*10+10
  for(minute in c(5,15,25,35,45,55)) {
     d.step<-d.step+1
     if(opt$step==hour+5) plot.forecast.hour(Year,Month,Day,hour+minute/60,d.step)
   }
}


