#!/usr/common/graphics/R/R-3.1.0/bin/R --no-save

# 1916 storm

library(GSDF.TWCR)
library(GSDF.WeatherMap)
library(parallel)

Year<-1916
Month<-3
Day<-26
Hour<-0
n.total<-24*6*4 # Total number of timesteps to be rendered
version<-'3.2.1'
fog.threshold<-exp(1)
members<-c(1,3,7,22,35,49)
#members<-c(1,3)

GSDF.cache.dir<-sprintf("%s/GSDF.cache",Sys.getenv('SCRATCH'))
if(!file.exists(GSDF.cache.dir)) dir.create(GSDF.cache.dir,recursive=TRUE)
Imagedir<-sprintf("%s/images/1916-storm-members.v2",Sys.getenv('SCRATCH'),version)
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

range<-15
aspect<-4/3
Options<-WeatherMap.set.option(Options,'lat.min',range*-1)
Options<-WeatherMap.set.option(Options,'lat.max',range)
Options<-WeatherMap.set.option(Options,'lon.min',range*aspect*-1)
Options<-WeatherMap.set.option(Options,'lon.max',range*aspect)
Options<-WeatherMap.set.option(Options,'pole.lon',173)
Options<-WeatherMap.set.option(Options,'pole.lat',36)

Options$obs.size<- 0.1

land<-WeatherMap.get.land(Options)

Options$mslp.lwd<-2
Options$mslp.base=0                    # Base value for anomalies
Options$mslp.range=50000                    # Anomaly for max contour
Options$mslp.step=250                       # Smaller -more contours
Options$mslp.tpscale=3500                    # Smaller -contours less transparent


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
           gp<-gpar(col=rgb(colour[1],colour[2],colour[3],tp*colour[4]),
                               lwd=Options$mslp.lwd*lwd,lty=lt)
           grid.xspline(x=unit(lines[[i]]$x,'native'),
                      y=unit(lines[[i]]$y,'native'),
                      shape=1,
                      gp=gp)
       }
    }
  }

plot.hour<-function(year,month,day,hour) {    

    image.name<-sprintf("%04d-%02d-%02d:%02d:%02d.png",year,month,day,as.integer(hour),
                                                         as.integer(hour%%1*60))
    ifile.name<-sprintf("%s/%s",Imagedir,image.name)
    #if(file.exists(ifile.name) && file.info(ifile.name)$size>0) return()

     png(ifile.name,
             width=1080*WeatherMap.aspect(Options),
             height=1080,
             bg=Options$sea.colour,
             pointsize=24,
             type='cairo')
    Options$label<-sprintf("%04d-%02d-%02d:%02d",year,month,day,as.integer(hour))
  
  	   pushViewport(dataViewport(c(Options$lon.min,Options$lon.max),
  				     c(Options$lat.min,Options$lat.max),
  				      extension=0))
  	      WeatherMap.draw.land(land,Options)
      obs<-TWCR.get.obs(year,month,day,hour,version=version)
      w<-which(obs$Longitude>180)
      obs$Longitude[w]<-obs$Longitude[w]-360
      WeatherMap.draw.obs(obs,Options)
       prmsl.normal<-TWCR.get.slice.at.hour('prmsl',year,month,day,hour,version='3.4.1',
                                               type='normal')
       e<-TWCR.get.members.slice.at.hour('prmsl',year,month,day,
                                  hour,version=version)
      m<-GSDF.select.from.1d(e,'ensemble',1)
      prmsl.normal<-GSDF.regrid.2d(prmsl.normal,m)
      for(vn in seq_along(members)) {
            m<-GSDF.select.from.1d(e,'ensemble',vn)
  	    m$data[]<-as.vector(m$data)-as.vector(prmsl.normal$data)
  	    Draw.pressure(m,Options,colour=c(0,0,0,1.0))
          }
      WeatherMap.draw.label(Options)
      gc()
    dev.off()
}

s<-NULL
for(n.count in seq(0,n.total)) {

    n.date<-c.date+n.count/(24*6) # 10-minute timesteps
    year<-as.numeric(as.character(years(n.date)))
    month<-months(n.date)
    day<-days(n.date)
    hour<-hours(n.date)+minutes(n.date)/60

    image.name<-sprintf("%04d-%02d-%02d:%02d:%02d.png",year,month,day,as.integer(hour),
                                                         as.integer(hour%%1*60))
    ifile.name<-sprintf("%s/%s",Imagedir,image.name)
    #if(file.exists(ifile.name) && file.info(ifile.name)$size>0) next
    # Each plot in a seperate parallel process
    mcparallel(plot.hour(year,month,day,hour))
    #plot.hour(year,month,day,hour)
    if(n.count%%24==0) mccollect(wait=TRUE)

}
mccollect()
