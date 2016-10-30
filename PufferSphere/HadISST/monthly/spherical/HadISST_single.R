#!/usr/bin/Rscript --no-save

# HadISST monthly (interpolated)
# Data are copied from
#  /project/hadobs1/OBS/marine/HadISST2/ice/
# and
# /project/hadobs1/OBS/marine/HadISST.2.1.0.0/

library(GSDF.PP)
library(GSDF.WeatherMap)
library(getopt)
library(lubridate)
library(RColorBrewer)

opt = getopt(c(
  'year',   'd', 2, "integer",
  'month',  'm', 2, "integer",
  'member', 'e', 1, "integer"
));
if ( is.null(opt$year) )   { stop("Year not specified") }
if ( is.null(opt$month) )  { stop("Month not specified") }
if ( is.null(opt$member) ) { opt$member<-137 }

Members<-c(1059,115,1169,1194,1346,137,1466,396,400,69)

Imagedir<-sprintf("%s/images/HadISST.2.1.red_blue",Sys.getenv('SCRATCH'))
if(!file.exists(Imagedir)) dir.create(Imagedir,recursive=TRUE)

Options<-WeatherMap.set.option(NULL)
Options<-WeatherMap.set.option(Options,'land.colour',rgb(0,0,0,255,
                                                       maxColorValue=255))
Options<-WeatherMap.set.option(Options,'sea.colour',rgb(250,250,250,255,
                                                       maxColorValue=255))
Options<-WeatherMap.set.option(Options,'ice.colour',rgb(255,255,255,100,
                                                       maxColorValue=255))
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
Options$ice.points<-100000

cols<-colorRampPalette(brewer.pal(11,"RdBu"))(100)

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
HadISST.get.sst.at.month<-function(year,month,member) {
  file<-sprintf("%s/HadISST.2.1.0.0/HadISST2_monthly_1x1_anomaly_realisation_%d.pp",
                Sys.getenv('SCRATCH'),member)
  field<-GSDF.PP.load(file,where=c(sprintf("lbyr==%d",year),
                             sprintf("lbmon==%d",month)))
  field$meta$pole.lon<-180
  w<-which(field$data< -100)
  is.na(field$data[w])<-TRUE
  return(field)
}
HadISST.get.ice.at.month<-function(year,month) {
   file<-sprintf("%s/HadISST.2.1.0.0/HadISST2.1.0.0_ice_monthly_1deg.pp",
                Sys.getenv('SCRATCH')) 
  if(year>1996) {
     file<-sprintf("%s/HadISST.2.1.0.0/NASA_ice_monthly_polarto1deg_9601-1212.pp",
                Sys.getenv('SCRATCH'))
   }
  field<-GSDF.PP.load(file,where=c(sprintf("lbyr==%d",year),
                             sprintf("lbmon==%d",month)))
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

sigmoid<-function(x) {
  s<-as.integer(100*(1 / (1 + exp(x))))+1
  s<-max(1,min(s,100))
  return(s)
}
set.temperature.colour<-function(data) {
    return(cols[sigmoid(data)])
}
Draw.temperature<-function(temperature,Options,Trange=5) {

  for(lon in seq_along(temperature$dimensions[[1]]$values)) {
    for(lat in seq_along(temperature$dimensions[[2]]$values)) {
      if(is.na(temperature$data[lon,lat,1,1])) next
      col<-set.temperature.colour(temperature$data[lon,lat,1,1])
      gp<-gpar(col=col,fill=col,alpha=temperature$alpha[lon,lat,1,1])
      x<-temperature$dimensions[[1]]$values[lon]
      dx<-(temperature$dimensions[[1]]$values[2]-temperature$dimensions[[1]]$values[1])*1.01
      if(x<Options$vp.lon.min-dx/2) x<-x+360
      if(x>Options$vp.lon.max+dx/2) x<-x-360
      y<-temperature$dimensions[[2]]$values[lat]
      dy<-(temperature$dimensions[[2]]$values[2]-temperature$dimensions[[2]]$values[1])*1.01
      p.x<-c(x-dx/2,x+dx/2,x+dx/2,x-dx/2)
      p.y<-c(y-dy/2,y-dy/2,y+dy/2,y+dy/2)
      p.r<-GSDF.ll.to.rg(p.y,p.x,Options$pole.lat,Options$pole.lon,polygon=TRUE)
      if(max(p.r$lon)<Options$vp.lon.min) p.r$lon<-p.r$lon+360
      if(min(p.r$lon)>Options$vp.lon.max) p.r$lon<-p.r$lon-360
      grid.polygon(x=unit(p.r$lon,'native'),
                   y=unit(p.r$lat,'native'),
                   gp=gp)
      if(min(p.r$lon)<Options$vp.lon.min) {
        p.r$lon<-p.r$lon+360
        grid.polygon(x=unit(p.r$lon,'native'),
                     y=unit(p.r$lat,'native'),
                     gp=gp)
      }
      if(max(p.r$lon)>Options$vp.lon.max) {
         p.r$lon<-p.r$lon-360
         grid.polygon(x=unit(p.r$lon,'native'),
                      y=unit(p.r$lat,'native'),
                      gp=gp)

      }
    }
  }
}


plot.field<-function(sst,ice,land,year,month,idx) {    

    image.name<-sprintf("%04d-%02d.%02d.png",year,month,idx)
    ifile.name<-sprintf("%s/%s",Imagedir,image.name)
    if(file.exists(ifile.name) && file.info(ifile.name)$size>0) return()

    Options<-set.pole(((year-1870)*12+month)*smooth*2+idx)
    land<-WeatherMap.get.land(Options)
    land<-GSDF:::GSDF.pad.longitude(land)
    
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
      ip<-WeatherMap.rectpoints(Options$ice.points,Options)
      Draw.temperature(sst,Options,Trange=4)
      Options<-WeatherMap.set.option(Options,'ice.colour',rgb(250,250,250,255,
                                                       maxColorValue=255))
      WeatherMap.draw.ice(ip$lat,ip$lon,ice,Options)
      WeatherMap.draw.land(land,Options)
      #WeatherMap.draw.label(Options)
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
  last.m<-HadISST.get.sst.at.month(last.year,last.month,member)
  m<-HadISST.get.sst.at.month(year,month,member)
  last.i<-HadISST.get.ice.at.month(last.year,last.month)
  i<-HadISST.get.ice.at.month(year,month)
  for(s in seq(1,smooth)) {
    d<-m
    d$data[]<-m$data*weights[s]+last.m$data*(1-weights[s])
    d.i<-i
    d.i$data[]<-i$data*weights[s]+last.i$data*(1-weights[s])
    plot.field(d,d.i,land,year,month,s)
  }
  next.year<-year
  next.month<-month+1
  if(next.month==13) {
    next.month<-1
    next.year<-year+1
  }
  next.m<-HadISST.get.sst.at.month(next.year,next.month,member)
  next.i<-HadISST.get.ice.at.month(next.year,next.month)
  for(s in seq(1,smooth)) {
    d<-m
    d$data[]<-m$data*weights[smooth-s+1]+next.m$data*(1-weights[smooth-s+1])
    d.i<-i
    d.i$data[]<-i$data*weights[smooth-s+1]+next.i$data*(1-weights[smooth-s+1])
    plot.field(d,d.i,land,year,month,s+smooth)
  }
  
}

plot.month(opt$year,opt$month,opt$member)

