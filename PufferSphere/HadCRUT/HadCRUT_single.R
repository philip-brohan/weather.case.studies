#!/usr/bin/Rscript --no-save

# HadCRUT4 monthly (interpolated)

library(GSDF)
library(GSDF.WeatherMap)
library(getopt)
library(lubridate)

opt = getopt(c(
  'year',   'd', 2, "integer",
  'month',  'm', 2, "integer",
  'member', 'e', 1, "integer"
));
if ( is.null(opt$year) )   { stop("Year not specified") }
if ( is.null(opt$month) )  { stop("Month not specified") }
if ( is.null(opt$member) ) { opt$member<-1 }


Imagedir<-sprintf("%s/images/HadCRUT4.red_blue",Sys.getenv('SCRATCH'))
if(!file.exists(Imagedir)) dir.create(Imagedir,recursive=TRUE)

Options<-WeatherMap.set.option(NULL)
Options<-WeatherMap.set.option(Options,'land.colour',rgb(0,0,0,255,
                                                       maxColorValue=255))
Options<-WeatherMap.set.option(Options,'sea.colour',rgb(100,100,100,255,
                                                       maxColorValue=255))
Options<-WeatherMap.set.option(Options,'ice.colour',Options$land.colour)
Options<-WeatherMap.set.option(Options,'background.resolution','high')

ptions<-WeatherMap.set.option(Options,'lat.min',-90)
Options<-WeatherMap.set.option(Options,'lat.max',90)
Options<-WeatherMap.set.option(Options,'lon.min',-190)
Options<-WeatherMap.set.option(Options,'lon.max',190)
Options$vp.lon.min<- -180
Options$vp.lon.max<-  180
Options$obs.size<- 0.5
Options<-WeatherMap.set.option(Options,'pole.lon',160)
Options<-WeatherMap.set.option(Options,'pole.lat',35)

set.pole<-function(step) {
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

HadCRUT.get.member.at.month<-function(year,month,member) {
  file<-sprintf("%s/HadCRUT4/HadCRUT.4.5.0.0.anomalies.%d.nc",
                Sys.getenv('SCRATCH'),member)
  field<-GSDF.ncdf.load(file,'temperature_anomaly',
                        time.range=ymd(sprintf("%04d-%02d-10",year,month),
                                       sprintf("%04d-%02d-20",year,month)),
                        lat.range=c(-90,90),lon.range=c(-180,360))
  field$meta$pole.lon<-180
  w<-which(field$data< -100)
  is.na(field$data[w])<-TRUE
  return(field)
}

Draw.temperature<-function(temperature,Options,Trange=5) {

  Options.local<-Options
  Options.local$fog.min.transparency<-1.0
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

plot.field<-function(field,land,year,month,idx) {    

    image.name<-sprintf("%04d-%02d.%02d.png",year,month,idx)
    ifile.name<-sprintf("%s/%s",Imagedir,image.name)
    if(file.exists(ifile.name) && file.info(ifile.name)$size>0) return()

    Options<-set.pole(((year-1870)*12+month)*smooth*2+idx)
    land<-WeatherMap.get.land(Options)
    
     png(ifile.name,
             width=1080*2,
             height=1080,
             bg=Options$sea.colour,
             pointsize=24,
             type='cairo')
    Options$label<-sprintf("%04d-%02d",year,month)
  
  	   pushViewport(dataViewport(c(Options$vp.lon.min,Options$vp.lon.max),
  				     c(Options$lat.min,Options$lat.max),
  				      extension=0))
      Draw.temperature(field,Options,Trange=4)
      WeatherMap.draw.land(land,Options)
      #WeatherMap.draw.label(Options)
      gc()
    dev.off()
}

# Interpolate smoothly between months
smooth<-3
s<-seq(1,smooth)
weights<-0.5+(s-0.5)*(0.5/smooth)
plot.month<-function(year,month,member) {    

  icount<-0
  for(i in seq(1,smooth*2)) {
     image.name<-sprintf("%04d-%02d.%02d.png",year,month,1)
     ifile.name<-sprintf("%s/%s",Imagedir,image.name)
     if(file.exists(ifile.name) && file.info(ifile.name)$size>0) icount<-icount+1
  }
  if(icount==smooth*2) return()

  land<-WeatherMap.get.land(Options)
  
  last.year<-year
  last.month<-month-1
  if(last.month==0) {
    last.month<-12
    last.year<-year-1
  }
  last.m<-HadCRUT.get.member.at.month(last.year,last.month,member)
  m<-HadCRUT.get.member.at.month(year,month,member)
  for(s in seq(1,smooth)) {
    d<-m
    d$data[]<-m$data*weights[s]+last.m$data*(1-weights[s])
    plot.field(d,land,year,month,s)
  }
  next.year<-year
  next.month<-month+1
  if(next.month==13) {
    next.month<-1
    next.year<-year+1
  }
  next.m<-HadCRUT.get.member.at.month(next.year,next.month,member)
  for(s in seq(1,smooth)) {
    d<-m
    d$data[]<-m$data*weights[smooth-s+1]+next.m$data*(1-weights[smooth-s+1])
    plot.field(d,land,year,month,s+smooth)
  }
  
}

plot.month(opt$year,opt$month,opt$member)

