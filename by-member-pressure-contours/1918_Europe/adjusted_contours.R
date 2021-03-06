#!/usr/common/graphics/R/R-3.1.0/bin/R --no-save

# Modern period

library(GSDF.TWCR)
library(GSDF.WeatherMap)
library(parallel)
library(lubridate)

Year<-1918
Month<-1
Day<-1
Hour<-0
d.total<-30 # Number of days to be rendered
version<-'3.5.1'
members<-seq(1,56)

GSDF.cache.dir<-sprintf("%s/GSDF.cache",Sys.getenv('SCRATCH'))
if(!file.exists(GSDF.cache.dir)) dir.create(GSDF.cache.dir,recursive=TRUE)
Imagedir<-sprintf("%s/images/1918-assimilation-adjusted",Sys.getenv('SCRATCH'),version)
if(!file.exists(Imagedir)) dir.create(Imagedir,recursive=TRUE)

c.date<-chron(dates=sprintf("%04d/%02d/%02d",Year,Month,Day),
          times=sprintf("%02d:00:00",Hour),
          format=c(dates='y/m/d',times='h:m:s'))

Options<-WeatherMap.set.option(NULL)
Options<-WeatherMap.set.option(Options,'show.mslp',T)
Options<-WeatherMap.set.option(Options,'show.ice',F)
Options<-WeatherMap.set.option(Options,'show.obs',T)
Options<-WeatherMap.set.option(Options,'show.fog',F)
Options<-WeatherMap.set.option(Options,'show.precipitation',F)
Options<-WeatherMap.set.option(Options,'temperature.range',6)

range<-45
aspect<-16/9
Options<-WeatherMap.set.option(Options,'lat.min',range*-1)
Options<-WeatherMap.set.option(Options,'lat.max',range)
Options<-WeatherMap.set.option(Options,'lon.min',range*aspect*-1)
Options<-WeatherMap.set.option(Options,'lon.max',range*aspect)
Options<-WeatherMap.set.option(Options,'pole.lon',185)
Options<-WeatherMap.set.option(Options,'pole.lat',15)

Options$obs.size<- 0.75

land<-WeatherMap.get.land(Options)

Options$mslp.lwd<-1
Options$mslp.base=0                         # Base value for anomalies
Options$mslp.range=50000              # Anomaly for max contour
Options$mslp.crange=3000              # Anomaly for max contour colour
Options$mslp.step=1000                # Smaller -more contours
Options$mslp.tpscale=350              # Smaller -contours less transparent
# Overrides mslp options options
contour.levels<-seq(-300,300,30)
contour.levels<-abs(contour.levels)**1.5*sign(contour.levels)

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
   value<-max(0.001,min(0.999,mp/length(contour.levels)))
   return(Options$wind.palette[ceiling(value*length(Options$wind.palette))])
}

Draw.obs.pressure<-function(obs,Options) {

  min.pressure<-min(contour.levels)
  w<-which(obs$SLP<min.pressure)
  if(length(w)>0) {
    Options$obs.colour<-obs.get.colour(0)
    WeatherMap.draw.obs(obs[w,],Options)
  }
  for(mp in seq(2,length(contour.levels))) {
    w<-which(obs$SLP<contour.levels[mp] & obs$SLP>=contour.levels[mp-1])
    if(length(w)>0) {
      Options$obs.colour<-obs.get.colour(mp-1)
      WeatherMap.draw.obs(obs[w,],Options)
    }
 }
  max.pressure<-Options$mslp.base+Options$mslp.crange
  w<-which(obs$SLP>max.pressure)
  if(length(w)>0) {
    Options$obs.colour<-obs.get.colour(length(contour.levels))
    WeatherMap.draw.obs(obs[w,],Options)
  }
}
 

Draw.pressure<-function(mslp,Options,colour=c(0,0,0,1)) {
  
    M<-GSDF.WeatherMap:::WeatherMap.rotate.pole(mslp,Options)
    lats<-M$dimensions[[GSDF.find.dimension(M,'lat')]]$values
    longs<-M$dimensions[[GSDF.find.dimension(M,'lon')]]$values
      # Need particular data format for contourLines
    if(lats[2]<lats[1] || longs[2]<longs[1] || max(longs)> 180 ) {
      if(lats[2]<lats[1]) lats<-rev(lats)
      if(longs[2]<longs[1]) longs<-rev(longs)
      longs[longs>180]<-longs[longs>180]-360
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
           lt<-1
           lwd<-1
           value<-min(0.999,(abs(lines[[i]]$level-Options$mslp.base)/
                      Options$mslp.crange))
           value<-value/2+0.5
           rgb<-col2rgb(Options$wind.palette[ceiling(value*length(Options$wind.palette))])/255
           gp<-gpar(col=rgb(rgb[1],rgb[2],rgb[3],tp*colour[4]),
                               lwd=Options$mslp.lwd*lwd,lty=lt)
           if(lines[[i]]$level<=Options$mslp.base) {
               lt<-1
               lwd<-1
           value<-min(0.999,(abs(lines[[i]]$level-Options$mslp.base)/
                      Options$mslp.crange))*-1
           value<-value/2+0.5
           rgb<-col2rgb(Options$wind.palette[ceiling(value*length(Options$wind.palette))])/255
           gp<-gpar(col=rgb(rgb[1],rgb[2],rgb[3],tp*colour[4]),
                               lwd=Options$mslp.lwd*lwd,lty=lt)
           }
             
           grid.xspline(x=unit(lines[[i]]$x,'native'),
                      y=unit(lines[[i]]$y,'native'),
                      shape=1,
                      gp=gp)
       }
    }
  }

plot.forecast.hour<-function(year,month,day,hour) {    

    image.name<-sprintf("%04d-%02d-%02d:%02d:%02d.99.png",year,month,day,as.integer(hour),
                                                         as.integer(hour%%1*60))
    ifile.name<-sprintf("%s/%s",Imagedir,image.name)
    if(file.exists(ifile.name) && file.info(ifile.name)$size>0) return()

     png(ifile.name,
             width=1080*WeatherMap.aspect(Options),
             height=1080,
             bg=Options$sea.colour,
             pointsize=24,
             type='cairo')
    Options$label<-sprintf("%04d-%02d-%02d:%02d:%02d",year,month,day,as.integer(hour),
                                                         as.integer(hour%%1*60))
  
  	   pushViewport(dataViewport(c(Options$lon.min,Options$lon.max),
  				     c(Options$lat.min,Options$lat.max),
  				      extension=0))
  	      WeatherMap.draw.land(land,Options)
      #obs<-TWCR.get.obs(year,month,day,hour,version=version)
      #w<-which(obs$Longitude>180)
      #obs$Longitude[w]<-obs$Longitude[w]-360
      #WeatherMap.draw.obs(obs,Options)
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
      WeatherMap.draw.label(Options)
      gc()
    dev.off()
}

plot.assimilation.stage<-function(year,month,day,hour,stage) {    

    image.name<-sprintf("%04d-%02d-%02d:%02d:%02d.%02d.png",year,month,day,as.integer(hour),
                                                         as.integer(hour%%1*60),as.integer(stage*100)+1)
    ifile.name<-sprintf("%s/%s",Imagedir,image.name)
    #if(file.exists(ifile.name) && file.info(ifile.name)$size>0) return()

     png(ifile.name,
             width=1080*WeatherMap.aspect(Options),
             height=1080,
             bg=Options$sea.colour,
             pointsize=24,
             type='cairo')
    Options$label<-sprintf("%04d-%02d-%02d:%02d:%02d",year,month,day,as.integer(hour),
                                                         as.integer(hour%%1*60))
  
  	   pushViewport(dataViewport(c(Options$lon.min,Options$lon.max),
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
      WeatherMap.draw.label(Options)
      gc()
    dev.off()
}

for(day.count in seq(0,d.total)) {

    dte<-ymd(sprintf("%04d-%02d-%02d",Year,Month,Day))+days(day.count)

    year<-year(dte)
    month<-month(dte)
    day<-day(dte)

    for(hour in c(0,6,12,18)) {
      for(stage in seq(0.05,0.95,0.1)) {
        mcparallel(plot.assimilation.stage(year,month,day,hour,stage))
        #plot.assimilation.stage(year,month,day,hour,stage)
        #q('no')
      }
      if(hour==6 || hour==18) mccollect(wait=TRUE)
    }
    mccollect(wait=TRUE)

    for(hour in seq(0,23)) {
      for(minute in c(5,15,25,35,45,55)) {
         mcparallel(plot.forecast.hour(year,month,day,hour+minute/60))
         #plot.forecast.hour(year,month,day,hour+minute/60)
       }
       if(hour%%4==3) mccollect(wait=TRUE)
    }
    mccollect(wait=TRUE)

}
