#!/usr/bin/Rscript --no-save

# Arctic 2013/4 single picture

library(GSDF.MERRA)
library(GSDF.WeatherMap)
library(parallel)
library(colorspace)

year<-2013
month<-12
day<-8
hour<-6

GSDF.cache.dir<-sprintf("%s/GSDF.cache",Sys.getenv('SCRATCH'))
Imagedir<-"."
use.cores<-1

c.date<-chron(dates=sprintf("%04d/%02d/%02d",year,month,day),
          times=sprintf("%02d:00:00",hour),
          format=c(dates='y/m/d',times='h:m:s'))

Options<-WeatherMap.set.option(NULL)
Options<-WeatherMap.set.option(Options,'show.mslp',F)
Options<-WeatherMap.set.option(Options,'show.ice',T)
Options<-WeatherMap.set.option(Options,'show.obs',F)
Options<-WeatherMap.set.option(Options,'show.fog',F)
Options<-WeatherMap.set.option(Options,'show.precipitation',T)
Options<-WeatherMap.set.option(Options,'temperature.range',12)
Options<-WeatherMap.set.option(Options,'obs.size',1)

range<-60
aspect<-4/4
Options<-WeatherMap.set.option(Options,'lat.min',range*-1)
Options<-WeatherMap.set.option(Options,'lat.max',range)
Options<-WeatherMap.set.option(Options,'lon.min',range*aspect*-1)
Options<-WeatherMap.set.option(Options,'lon.max',range*aspect)
Options<-WeatherMap.set.option(Options,'pole.lon',115)
Options<-WeatherMap.set.option(Options,'pole.lat',1)
Options$sea.colour=rgb(80*1.5,95*1.5,107*1.5,255,
                       maxColorValue=255)   # For background
Options$ice.colour=rgb(150*1.2,165*1.2,177*1.2,255,
                maxColorValue=255)
Options$merge.colour=NULL
Options$wind.palette.maxgrey<-10000
#Options$wind.palette<-diverge_hcl(7, c = 100, l = c(50, 90), power = 1)
Options$wind.palette<-diverge_hcl(70, c = 50, l = 25, power = 1)

Options$wind.vector.seed<-Options$wind.vector.seed*0.45*1.5
Options$ice.points<-100000
Options$wind.palette.opacity<-1.0
Options$wind.vector.scale<-0.3
Options$wind.vector.lwd<-3
Options$mslp.range=50000                    # Anomaly for max contour
Options$mslp.step=500                       # Smaller -more contours
Options$mslp.tpscale=500                    # Smaller -contours less transparent
Options$mslp.lwd=1
land<-WeatherMap.get.land(Options)

make.streamlines<-function(n.count,streamlines=NULL) {

    n.date<-c.date+n.count/24
    year<-as.numeric(as.character(years(n.date)))
    month<-months(n.date)
    day<-days(n.date)
    hour<-hours(n.date)

    sf.name<-sprintf("%s/streamlines.%04d-%02d-%02d:%02d.rd",
                           Imagedir,year,month,day,hour)
    if(file.exists(sf.name) && file.info(sf.name)$size>0) {
       load(sf.name)
       return(s)
    }

    uwnd<-MERRA.get.slice.at.hour('U10M',year,month,day,hour)
    if(is.null(uwnd)) stop("MERRA data retrieval failed")
    vwnd<-MERRA.get.slice.at.hour('V10M',year,month,day,hour)
    if(is.null(vwnd)) stop("MERRA data retrieval failed")
    t.actual<-MERRA.get.slice.at.hour('T2M',year,month,day,hour)
    if(is.null(t.actual)) stop("MERRA data retrieval failed")
    t.normal<-t.actual
    t.normal$data[]<-rep(283,length(t.normal$data))
    s<-WeatherMap.make.streamlines(streamlines,uwnd,vwnd,t.actual,t.normal,Options)
    save(year,month,day,hour,s,file=sf.name)
    gc(verbose=FALSE)
    return(s)

}
plot.hour<-function(core) {    

    l.count<-n.count+core
    n.date<-c.date+l.count/24
    year<-as.numeric(as.character(years(n.date)))
    month<-months(n.date)
    day<-days(n.date)
    hour<-hours(n.date)

    image.name<-sprintf("%04d-%02d-%02d:%02d.pdf",year,month,day,hour)

    ifile.name<-sprintf("%s/%s",Imagedir,image.name)
    if(file.exists(ifile.name) && file.info(ifile.name)$size>0) return()
    print(sprintf("%d %04d-%02d-%02d:%02d - %s",l.count,year,month,day,hour,
                   Sys.time()))

    prmsl<-MERRA.get.slice.at.hour('SLP',year,month,day,hour)
    if(is.null(prmsl)) stop("MERRA data retrieval failed")
    prate<-NULL
    if(Options$show.precipitation) {
       prate<-MERRA.get.slice.at.hour('PRECTOT',year,month,day,hour)
       if(is.null(prate)) stop("MERRA data retrieval failed")
       prate$data[]<-prate$data*2
     }
    icec<-MERRA.get.slice.at.hour('FRSEAICE',year,month,day,hour)
    if(is.null(icec)) stop("MERRA data retrieval failed")
    obs<-NULL

   #  png(ifile.name,
   #          width=1080*WeatherMap.aspect(Options),
   #          height=1080,
   #          bg=Options$sea.colour,
   #          pointsize=24,
   #          type='cairo')
    pdf(file=ifile.name,
             width=8.25*WeatherMap.aspect(Options),
             height=8.25,
             bg=Options$sea.colour,
             pointsize=12)
    Options$label<-sprintf("%04d-%02d-%02d:%02d",year,month,day,hour)
       WeatherMap.draw(Options=Options,uwnd=NULL,icec=icec,
                          vwnd=NULL,precip=prate,mslp=prmsl,
                          t.actual=NULL,t.normal=NULL,land=land,
                          fog=fog,obs=obs,streamlines=stlist[[core]])
    dev.off()
}

stlist<-list()
stlist[[use.cores+2]]<-3

#for(n.count in seq(0,24*150,use.cores)) {
for(n.count in seq(0,0,use.cores)) { # Reset cores to 8

  stlist[[1]]<-stlist[[use.cores+1]]
  for(core in seq(2,use.cores+1)) {
    stlist[[core]]<-make.streamlines(n.count+core-1,streamlines=stlist[[core-1]])
  }
  
  #r<-mclapply(seq(2,use.cores+1),plot.hour,mc.cores=use.cores,mc.preschedule=FALSE)
  lapply(seq(2,use.cores+1),plot.hour)
  gc(verbose=FALSE)
  
}
