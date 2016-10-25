#!/usr/bin/Rscript --no-save

# Just make the streamlines for later rendering.
# Spherical version for puffersphere (fewer, bigger streamlines)

library(GSDF.TWCR)
library(GSDF.WeatherMap)
library(grid)
library(deldir)
library(RColorBrewer)
library(lubridate)

Year<-2014
Month<-1
Day<-31
Hour<-1
n.total<-334*24
version<-'3.5.1'

c.date<-ymd_hms(sprintf("%04d-%02d-%02d:%02d:00:00",Year,Month,Day,Hour))


GSDF.cache.dir<-sprintf("%s/GSDF.cache",Sys.getenv('SCRATCH'))
if(!file.exists(GSDF.cache.dir)) dir.create(GSDF.cache.dir,recursive=TRUE)
Imagedir<-sprintf("%s/images/TWCR_multivariate_spherical",Sys.getenv('SCRATCH'))
if(!file.exists(Imagedir)) dir.create(Imagedir,recursive=TRUE)

Options<-WeatherMap.set.option(NULL)
Options<-WeatherMap.set.option(Options,'pole.lon',160)
Options<-WeatherMap.set.option(Options,'pole.lat',35)

Options<-WeatherMap.set.option(Options,'lat.min',-90)
Options<-WeatherMap.set.option(Options,'lat.max',90)
Options<-WeatherMap.set.option(Options,'lon.min',-190)
Options<-WeatherMap.set.option(Options,'lon.max',190)
Options$vp.lon.min<- -180
Options$vp.lon.max<-  180
Options<-WeatherMap.set.option(Options,'wrap.spherical',T)

Options<-WeatherMap.set.option(Options,'wind.vector.points',3)
Options<-WeatherMap.set.option(Options,'wind.vector.scale',0.3)
Options<-WeatherMap.set.option(Options,'wind.vector.move.scale',1)
Options<-WeatherMap.set.option(Options,'wind.vector.density',2)
Options$ice.points<-100000

set.pole<-function(step,Options) {
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

# Apply appropriate longitude boundary conditions
periodic.boundary<-function(s,Options) {
    w<-which(s$x[,1]>Options$vp.lon.min | s$x[,1]<Options$vp.lon.max)
    if(length(w)>0) {
       for(var in c('status','t_anom','magnitude','id')) {
           s[[var]]<-s[[var]][w]
         }
       for(var in c('x','y','shape')) {
           s[[var]]<-s[[var]][w,]
       }
     }
     w<-which(s$x[,1]<Options$vp.lon.min+(Options$vp.lon.min-Options$lon.min))
     if(length(w)>0) {
        s[['x']]<-rbind(s[['x']],s[['x']][w,]+360)
        for(var in c('status','t_anom','magnitude','id')) {
           s[[var]]<-c(s[[var]],s[[var]][w])
        }
        for(var in c('y','shape')) {
           s[[var]]<-rbind(s[[var]],s[[var]][w,])
        }
      }
     w<-which(s$x[,1]>Options$vp.lon.max-(Options$lon.max-Options$vp.lon.max))
     if(length(w)>0) {
        s[['x']]<-rbind(s[['x']],s[['x']][w,]-360)
        for(var in c('status','t_anom','magnitude','id')) {
           s[[var]]<-c(s[[var]],s[[var]][w])
        }
        for(var in c('y','shape')) {
           s[[var]]<-rbind(s[[var]],s[[var]][w,])
        }
      }
     return(s)
}

make.streamlines<-function(year,month,day,hour,Options,streamlines=NULL) {

    sf.name<-sprintf("%s/streamlines.%04d-%02d-%02d:%02d.rd",
                           Imagedir,year,month,day,hour)
    if(file.exists(sf.name) && file.info(sf.name)$size>5000) {
       load(sf.name)
       return(s)
    }
    print(sprintf("%04d-%02d-%02d:%02d - %s",year,month,day,hour,
                   Sys.time()))

    uwnd<-TWCR.get.slice.at.hour('uwnd.10m',year,month,day,hour,version=version)
    vwnd<-TWCR.get.slice.at.hour('vwnd.10m',year,month,day,hour,version=version)
    t.actual<-TWCR.get.slice.at.hour('air.2m',year,month,day,hour,version=version)
    t.normal<-t.actual
    t.normal$data[]<-rep(286,length(t.normal$data))
    s<-WeatherMap.make.streamlines(streamlines,uwnd,vwnd,t.actual,t.normal,Options)
    if(is.null(streamlines)) s$status<-s$status*0+5
    s<-periodic.boundary(s,Options)
    save(year,month,day,hour,s,file=sf.name)
    gc(verbose=FALSE)
    return(s)

}

s<-NULL
for(n.count in seq(0,n.total)) {

    n.date<-c.date+days(as.integer(n.count/24))
    year<-year(n.date)
    month<-month(n.date)
    day<-day(n.date)
    hour<-(n.count+Hour)%%24

    step<-as.integer(ymd(sprintf("%04d-%02d-%02d",year,month,day))-ymd("2014-01-01"))*24+hour
    Options<-set.pole(step,Options)

    # serial component - streamlines evolve from hour to hour
    s<-make.streamlines(year,month,day,hour,Options,streamlines=s)

}
