#!/usr/bin/Rscript --no-save

# Show surface observations network - coloured by pressure anomaly.
# Spherical version with garish colours
# Move the north pole to centre UK - shows up better on PufferSphere?

library(GSDF.WeatherMap)
library(GSDF.TWCR)
library(GSDF)
library(parallel)
library(lubridate)
library(getopt)
library(RColorBrewer)

opt = getopt(c(
  'year',   'y', 2, "integer",
  'month',  'm', 2, "integer",
  'day',    'd', 2, "integer",
  'hour',   'h', 2, "integer" 
));
if ( is.null(opt$year  ) )   { stop("Year not specified") }
if ( is.null(opt$month  ) )  { stop("Month not specified") }
if ( is.null(opt$day  ) )    { stop("Day not specified") }
if ( is.null(opt$hour  ) )   { stop("Hour not specified") }

version<-'3.5.1'

hourly.obs.dir<-sprintf("%s/20CR/version_3.5.1/observations.by.hour",Sys.getenv('SCRATCH'))

Imagedir<-sprintf("%s/images/ISPD_by_pressure.spherical",Sys.getenv('SCRATCH'))
if(!file.exists(Imagedir)) dir.create(Imagedir,recursive=TRUE)

Options<-WeatherMap.set.option(NULL)
Options<-WeatherMap.set.option(Options,'lat.min',-90)
Options<-WeatherMap.set.option(Options,'lat.max',90)
Options<-WeatherMap.set.option(Options,'lon.min',-190)
Options<-WeatherMap.set.option(Options,'lon.max',190)
Options$vp.lon.min<- -180
Options$vp.lon.max<-  180
Options<-WeatherMap.set.option(Options,'wrap.spherical',T)
Options<-WeatherMap.set.option(Options,'obs.size',1.5)
Options<-WeatherMap.set.option(Options,'obs.colour',rgb(255,215,0,255,
                                                       maxColorValue=255))
Options<-WeatherMap.set.option(Options,'land.colour',rgb(0,0,0,255,
                                                       maxColorValue=255))
Options<-WeatherMap.set.option(Options,'sea.colour',rgb(100,100,100,255,
                                                       maxColorValue=255))
Options<-WeatherMap.set.option(Options,'pole.lon',160)
Options<-WeatherMap.set.option(Options,'pole.lat',35)
Options<-WeatherMap.set.option(Options,'background.resolution','high')

Options$obs.size<- 2

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
  x<-x/1000 # want range about -5<->5, scale for Pa anomalies
  s<-as.integer(100*(1 / (1 + exp(x))))+1
  s<-max(1,min(s,100))
  return(s)
}
set.obs.colour<-function(data) {
    return(cols[sigmoid(data)])
}
set.pole<-function(year,month,day,hour,Options) {
  step<-as.integer(ymd(sprintf("%04d-%02d-%02d",year,month,day))-
                   ymd(sprintf("%04d-01-01",year)))*24+hour
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

load.hourly.obs<-function(year,month,day,hour,spread=12) {
  result<-NULL
  for(hr.offset in seq(spread*-1,spread)) {
    hr.local<-hour+hr.offset
    fn<-''
    if(as.integer(hr.local)<0) {
      dte<-ymd(sprintf("%04d%02d%02d",year,month,day))-days(1)
      hr.local<-hr.local+24
      fn<-sprintf("%s/%04d%02d%02d%02d.Rdata",hourly.obs.dir,year(dte),
                           month(dte),day(dte),as.integer(hr.local))
      hr.local<- hr.local-24
    } 
    if(as.integer(hr.local)>23) {
      dte<-ymd(sprintf("%04d%02d%02d",year,month,day))+days(1)
      hr.local<-hr.local-24
      fn<-sprintf("%s/%04d%02d%02d%02d.Rdata",hourly.obs.dir,year(dte),
                           month(dte),day(dte),as.integer(hr.local))
      hr.local<-hr.local+24
    } 
    if(as.integer(hr.local)<=23 && as.integer(hr.local)>=0) {
      fn<-sprintf("%s/%04d%02d%02d%02d.Rdata",hourly.obs.dir,year,
                           month,day,as.integer(hr.local))
    } 
    if(!file.exists(fn)) {
      warning(sprintf("No obs file %s",fn))
      next
    }
    load(fn)
    obs.hr$Time.Offset<-obs.hr$Time.Offset-min(obs.hr$Time.Offset,na.rm=TRUE)+hr.offset
    if(is.null(result)) result<-obs.hr
    else result<-rbind(result,obs.hr)
  }
  w<-which(result$Longitude>180)
  if(length(w)>0) result$Longitude[w]<-result$Longitude[w]-360
  return(result)
}

Draw.obs.pressure<-function(obs,Options) {

  min.pressure<-Options$mslp.base-Options$mslp.crange
  w<-which(obs$SLP<min.pressure)
  if(length(w)>0) {
    Options$obs.colour<-set.obs.colour(min.pressure)
    WeatherMap.draw.obs(obs[w,],Options)
  }
  for(mp in seq(Options$mslp.base-Options$mslp.crange+Options$mslp.step,
                Options$mslp.base+Options$mslp.crange,Options$mslp.step)) {
    w<-which(obs$SLP<mp & obs$SLP>=mp-Options$mslp.step)
    if(length(w)>0) {
      Options$obs.colour<-set.obs.colour(mp-Options$mslp.step/2)
      WeatherMap.draw.obs(obs[w,],Options)
    }
 }
  max.pressure<-Options$mslp.base+Options$mslp.crange
  w<-which(obs$SLP>max.pressure)
  if(length(w)>0) {
    Options$obs.colour<-set.obs.colour(max.pressure)
    WeatherMap.draw.obs(obs[w,],Options)
  }
}

plot.hour<-function(year,month,day,hour) {

    image.name<-sprintf("%04d-%02d-%02d:%02d.png",year,month,day,hour)

    ifile.name<-sprintf("%s/%s",Imagedir,image.name)
    if(file.exists(ifile.name) && file.info(ifile.name)$size>0) return()

    Options<-set.pole(year,month,day,hour,Options)
    land<-WeatherMap.get.land(Options)

    obs<-load.hourly.obs(year,month,day,hour)
    w<-which(obs$Longitude>180)
    obs$Longitude[w]<-obs$Longitude[w]-360
    prmsl.normal<-TWCR.get.slice.at.hour('prmsl',year,month,day,hour,version='3.4.1',
                                               type='normal')
    obs$SLP<-obs$SLP*100-GSDF.interpolate.ll(prmsl.normal,obs$Latitude,obs$Longitude)
    #obs<-obs[order(abs(Time.Offset),decreasing=TRUE),]
    
     png(ifile.name,
             width=1080*2,
             height=1080,
             bg=Options$sea.colour,
             pointsize=24,
             type='cairo')
          pushViewport(dataViewport(c(Options$vp.lon.min,Options$vp.lon.max),
                                    c(Options$lat.min,Options$lat.max),
                                    extension=0))
          WeatherMap.draw.land(land,Options)
         for(hr.offset in seq(11,0,-1)) {
            w<-which(abs(obs$Time.Offset)>=hr.offset &
                     abs(obs$Time.Offset)<(hr.offset+1))
            if(length(w)==0) next
            Draw.obs.pressure(obs[w,],Options)
          }
         upViewport()
    dev.off()
}

plot.hour(opt$year,opt$month,opt$day,opt$hour)
