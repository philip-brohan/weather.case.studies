#!/usr/bin/Rscript

# Pressure and ice only - for the PufferSphere
# Run a single timestep only - designed to be run in parallel on SPICE.
# Historical version with fog - opaque fog because of the puffersphere's
#  limited brightness and colours.

library(GSDF.TWCR)
library(GSDF.WeatherMap)
library(parallel)
library(getopt)

opt = getopt(c(
  'step',   's', 2, "integer"
));
if ( is.null(opt$step  ) )   { stop("Step not specified") }

Year<-1916
Month<-1
Day<-2
Hour<-0
version<-'3.5.1'
member<-12
fog.threshold<-exp(1)

Imagedir<-sprintf("%s/images/P+I_rotating_1916",Sys.getenv('SCRATCH'))
if(!file.exists(Imagedir)) dir.create(Imagedir,recursive=TRUE)

c.date<-chron(dates=sprintf("%04d/%02d/%02d",Year,Month,Day),
          times=sprintf("%02d:00:00",Hour),
          format=c(dates='y/m/d',times='h:m:s'))

Options<-WeatherMap.set.option(NULL)

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
Options<-WeatherMap.set.option(Options,'obs.colour',rgb(238,232,170,255,
                                                       maxColorValue=255))
Options$mslp.base=101325                    # Base value for anomalies
Options$mslp.range=50000                    # Anomaly for max contour
Options$mslp.step=500                       # Smaller -> more contours
Options$mslp.tpscale=500                    # Smaller -> contours less transparent
Options$mslp.lwd=5
Options$fog.colour<-c(0.65,0.65,0.65)        # 0-1, bigger -> lighter fog
Options$fog.min.transparency<-0.95           # 0-1, bigger -> thicker fog
Options<-WeatherMap.set.option(Options,'obs.size',1.5)

Options$ice.points<-100000

get.member.at.hour<-function(variable,year,month,day,hour,member) {

       t<-TWCR.get.members.slice.at.hour(variable,year,month,day,
                                  hour,version=version)
       t<-GSDF.select.from.1d(t,'ensemble',member)
       gc()
       return(t)
}

set.pole<-function(step,Options) {
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

Draw.pressure<-function(mslp,Options,colour=c(0,0,0)) {

  M<-GSDF.WeatherMap:::WeatherMap.rotate.pole(mslp,Options)
  lats<-M$dimensions[[GSDF.find.dimension(M,'lat')]]$values
  longs<-M$dimensions[[GSDF.find.dimension(M,'lon')]]$values
    # Need particular data format for contourLines
  maxl<-Options$vp.lon.max
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
  M<-GSDF:::GSDF.pad.longitude(M) # Extras for periodic boundary conditions
  lats<-M$dimensions[[GSDF.find.dimension(M,'lat')]]$values
  longs<-M$dimensions[[GSDF.find.dimension(M,'lon')]]$values
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
         grid.xspline(x=unit(lines[[i]]$x,'native'),
                    y=unit(lines[[i]]$y,'native'),
                    shape=1,
                    gp=gp)
     }
  }
}

plot.hour<-function(year,month,day,hour,step) {    

    image.name<-sprintf("%04d-%02d-%02d:%02d.png",year,month,day,hour)

    ifile.name<-sprintf("%s/%s",Imagedir,image.name)
    if(file.exists(ifile.name) && file.info(ifile.name)$size>0) return()

    prmsl.T<-get.member.at.hour('prmsl',year,month,day,hour,member=member )
    icec<-TWCR.get.slice.at.hour('icec',year,month,day,hour,version=version)

    prmsl.spread<-TWCR.get.slice.at.hour('prmsl',year,month,day,hour,version=version,
                                              type='spread')
    prmsl.sd<-TWCR.get.slice.at.hour('prmsl',year,month,day,hour,
                                         version='3.4.1',type='standard.deviation')
    prmsl.normal<-TWCR.get.slice.at.hour('prmsl',year,month,day,hour,version='3.4.1',
                                             type='normal')
    prmsl<-GSDF.regrid.2d(prmsl.T,prmsl.spread)
    fog<-TWCR.relative.entropy(prmsl.normal,prmsl.sd,prmsl,prmsl.spread)
    #fog$data[]<-1-pmin(fog.threshold,pmax(0,fog$data))/fog.threshold
    w<-which(fog$data<fog.threshold)
    fog$data[w]<-1
    fog$data[-w]<-0

    Options<-set.pole(step,Options)
    land<-WeatherMap.get.land(Options)
    land<-GSDF:::GSDF.pad.longitude(land)

     png(ifile.name,
             width=2160,
             height=1080,
             bg=Options$sea.colour,
             pointsize=24,
             type='cairo')
       pushViewport(dataViewport(c(Options$vp.lon.min,Options$vp.lon.max),
                                 c(Options$lat.min,Options$lat.max),
                                  extension=0))
          ip<-WeatherMap.rectpoints(Options$ice.points,Options)
          WeatherMap.draw.ice(ip$lat,ip$lon,icec,Options)
          WeatherMap.draw.land(land,Options)
          obs<-TWCR.get.obs(year,month,day,hour,version=version)
          WeatherMap.draw.obs(obs,Options)
    
          #Draw.pressure(prmsl.T,Options,colour=c(0.91,0.46,0))
          Draw.pressure(prmsl.T,Options,colour=c(255/255,70/255,0))
          WeatherMap.draw.fog(fog,Options)

     upViewport()

    dev.off()
}

n.count <-opt$step

    n.date<-c.date+n.count/24
    year<-as.numeric(as.character(years(n.date)))
    month<-months(n.date)
    day<-days(n.date)
    hour<-n.count%%24

    image.name<-sprintf("%04d-%02d-%02d:%02d.png",year,month,day,hour)
    ifile.name<-sprintf("%s/%s",Imagedir,image.name)
    plot.hour(year,month,day,hour,n.count)

    #system(sprintf("mogrify -gamma 0.6 %s",ifile.name))
