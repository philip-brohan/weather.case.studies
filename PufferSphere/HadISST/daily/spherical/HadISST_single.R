#!/usr/bin/Rscript --no-save

# HadISST monthly (interpolated)
# Data are copied from
#  /project/hadobs1/OBS/marine/HadISST2/ice/
# and
# /project/hadobs1/OBS/marine/HadISST.2.1.0.0/

library(GSDF)
library(GSDF.WeatherMap)
library(getopt)
library(lubridate)
library(RColorBrewer)

opt = getopt(c(
  'year',   'y', 2, "integer",
  'month',  'm', 2, "integer",
  'day',  'd', 2, "integer",
  'member', 'e', 1, "integer"
));
if ( is.null(opt$year) )   { stop("Year not specified") }
if ( is.null(opt$month) )  { stop("Month not specified") }
if ( is.null(opt$day) )    { stop("Day not specified") }
if ( is.null(opt$member) ) { opt$member<-1 }

Imagedir<-sprintf("%s/images/HadISST.2.2.daily.spherical",Sys.getenv('SCRATCH'))
if(!file.exists(Imagedir)) dir.create(Imagedir,recursive=TRUE)

Options<-WeatherMap.set.option(NULL)
Options<-WeatherMap.set.option(Options,'land.colour',rgb(0,0,0,255,
                                                       maxColorValue=255))
Options<-WeatherMap.set.option(Options,'sea.colour',rgb(150,150,150,255,
                                                       maxColorValue=255))
Options<-WeatherMap.set.option(Options,'ice.colour',rgb(255,255,255,100,
                                                       maxColorValue=255))
Options<-WeatherMap.set.option(Options,'background.resolution','high')

ptions<-WeatherMap.set.option(Options,'lat.min',-90)
Options<-WeatherMap.set.option(Options,'lat.max',90)
Options<-WeatherMap.set.option(Options,'lon.min',-190)
Options<-WeatherMap.set.option(Options,'lon.max',190)
Options$vp.lon.min<- -180
Options$vp.lon.max<- 180
Options$obs.size<- 0.5
Options<-WeatherMap.set.option(Options,'pole.lon',160)
Options<-WeatherMap.set.option(Options,'pole.lat',35)
Options$ice.points<-400000

cols.base<-brewer.pal(11,"RdBu")
cols.base<-c(cols.base[1:3],"#C0C0C0",cols.base[9:11]) # Filter out the white bits
cols<-cols.base

set.pole<-function(step) {
  if(step<=1000) return(Options)
  lon<-160+((step-1000)/10)
  if(lon>360) lon<-lon%%360
  lat<-35+sin((step-1000)/500)*20
  Options<-WeatherMap.set.option(Options,'pole.lon',lon)
  Options<-WeatherMap.set.option(Options,'pole.lat',lat)
  min.lon<-((step-1000)/5)%%360-180
  Options<-WeatherMap.set.option(Options,'lon.min',min.lon-10)
  Options<-WeatherMap.set.option(Options,'lon.max',min.lon+370)
  Options<-WeatherMap.set.option(Options,'vp.lon.min',min.lon   )
  Options<-WeatherMap.set.option(Options,'vp.lon.max',min.lon+360)
  return(Options)
}
HadISST.get.sst.at.month<-function(year,month,day,member) {
  file<-sprintf("%s/HadISST.2.2.0.0/Realisation_%d/HadISST2_preliminary_frompentad_qxqxd_SST_and_ice_%d_%d_%04d.nc",
                Sys.getenv('SCRATCH'),member,day,month,year)
  field<-GSDF.ncdf.load(file,'sst',
                        time.range=ymd(sprintf("%04d-%02d-01",1800,1),
                                       sprintf("%04d-%02d-31",2050,12)),
                        lat.range=c(-90,90),lon.range=c(-180,360))
  if(month==2 && day==29) day==28
  climf.name<-sprintf("%s/HadISST.2.2.0.0/climatology_%02d%02d.Rdata",
                            Sys.getenv('SCRATCH'),month,day)
  clm<-readRDS(climf.name)
  field$data[]<-field$data-clm$data
  field$meta$pole.lon<-180
  w<-which(field$data< -100)
  is.na(field$data[w])<-TRUE
  field$dimensions[[2]]$values<-field$dimensions[[2]]$values*-1
  return(field)
}
HadISST.get.ice.at.month<-function(year,month,day,member) {
  file<-sprintf("%s/HadISST.2.2.0.0/Realisation_%d/HadISST2_preliminary_frompentad_qxqxd_SST_and_ice_%d_%d_%04d.nc",
                Sys.getenv('SCRATCH'),member,day,month,year)
  field<-GSDF.ncdf.load(file,'sic',
                        time.range=ymd(sprintf("%04d-%02d-01",1800,1),
                                       sprintf("%04d-%02d-31",2050,12)),
                        lat.range=c(-90,90),lon.range=c(-180,360))
  field$meta$pole.lon<-180
  w<-which(field$data< -100)
  is.na(field$data[w])<-TRUE
  field$dimensions[[2]]$values<-field$dimensions[[2]]$values*-1
  return(field)
}

sigmoid<-function(x) {
  s<-1-(1 / (1 + exp(x)))
 return(s)
}
Draw.temperature<-function(temperature,Options,Trange=5) {

  Options.local<-Options
  Options.local$fog.min.transparency<-1.0
  tplus<-temperature
  #tplus$data[]<-pmax(0,pmin(Trange,tplus$data))/Trange
  tplus$data[]<-pmax(0,(sigmoid(tplus$data)-0.5)*2)
  Options.local$fog.colour<-c(1,0,0)
  WeatherMap.draw.fog(tplus,Options.local)
  tminus<-temperature
  tminus$data[]<-tminus$data*-1
  tminus$data[]<-pmax(0,(sigmoid(tminus$data)-0.5)*2)
  Options.local$fog.colour<-c(0,0,1)
  WeatherMap.draw.fog(tminus,Options.local)
}

set.temperature.colour<-function(data) {
    return(cols[sigmoid(data)])
}
Draw.temperature2<-function(temperature,Options,Trange=5) {

  for(lon in seq_along(temperature$dimensions[[1]]$values)) {
    for(lat in seq_along(temperature$dimensions[[2]]$values)) {
      if(is.na(temperature$data[lon,lat,1])) next
      col<-set.temperature.colour(temperature$data[lon,lat,1])
      gp<-gpar(col=col,fill=col,alpha=temperature$alpha[lon,lat,1])
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

plot.field<-function(sst,ice,land,year,month,day) {    

    image.name<-sprintf("%04d-%02d-%02d.png",year,month,day)
    ifile.name<-sprintf("%s/%s",Imagedir,image.name)
    if(file.exists(ifile.name) && file.info(ifile.name)$size>0) return()

    Options<-set.pole(interval(ymd("1961-01-01"),ymd(sprintf("%04d-%02d-%02d",year,month,day)))/ddays(1))
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
      Options<-WeatherMap.set.option(Options,'ice.colour',rgb(250,250,250,255,
                                                       maxColorValue=255))
      WeatherMap.draw.ice(ip$lat,ip$lon,ice,Options)
      Draw.temperature(sst,Options,Trange=4)
      WeatherMap.draw.land(land,Options)
      bak<-Options$land.colour
    dev.off()
}

plot.day<-function(year,month,day,member) {    

  
  m<-HadISST.get.sst.at.month(year,month,day,member)
  i<-HadISST.get.ice.at.month(year,month,day,member)
  plot.field(m,i,land,year,month,day)
  
}

plot.day(opt$year,opt$month,opt$day,opt$member)

