#!/usr/common/usg/R/R-3.0.1/bin/Rscript --no-save

# Alternative look at the 2013/14 polar vortex

library(GSDF.MERRA)
library(GSDF.WeatherMap)
library(parallel)
library(getopt)

opt = getopt(c(
  'year',   'y', 2, "integer",
  'month',   'm', 2, "integer",
  'nmonths','n', 2, "integer"
));
if ( is.null(opt$year  ) )   { stop("Year not specified") }
if ( is.null(opt$month  ) )   { stop("Month not specified") }
if ( is.null(opt$nmonths ) ) { opt$nmonths  = 6 }

Year<-opt$year
Month<-opt$month
Day<-1
Hour<-0
n.total<-opt$nmonths*31*24 # Total number of hours to be rendered
version<-opt$version
fog.threshold<-exp(1)

GSDF.cache.dir<-sprintf("%s/GSDF.cache",Sys.getenv('GSCRATCH'))
Imagedir<-sprintf("%s/images/pv.anomalies.sd",Sys.getenv('GSCRATCH'))
if(!file.exists(Imagedir)) dir.create(Imagedir,recursive=TRUE)

c.date<-chron(dates=sprintf("%04d/%02d/%02d",Year,Month,Day),
          times=sprintf("%02d:00:00",Hour),
          format=c(dates='y/m/d',times='h:m:s'))

Options<-WeatherMap.set.option(NULL)

range<-60
aspect<-4/3
Options<-WeatherMap.set.option(Options,'lat.min',range*-1)
Options<-WeatherMap.set.option(Options,'lat.max',range)
Options<-WeatherMap.set.option(Options,'lon.min',range*aspect*-1)
Options<-WeatherMap.set.option(Options,'lon.max',range*aspect)
Options<-WeatherMap.set.option(Options,'pole.lon',115)
Options<-WeatherMap.set.option(Options,'pole.lat',1)
Options$label.yp=0.03                  
Options$mslp.base=0                    # Base value for anomalies
#Options$mslp.base=101325                    # Base value for anomalies
Options$mslp.range=50000                    # Anomaly for max contour
Options$mslp.step=500                       # Smaller -> more contours
Options$mslp.tpscale=500                    # Smaller -> contours less transparent
Options$mslp.lwd=2
Options<-WeatherMap.set.option(Options,'obs.size',0.5)
Options<-WeatherMap.set.option(Options,'obs.colour',rgb(255,215,0,255,
                                                       maxColorValue=255))

Options$ice.points<-50000
land<-WeatherMap.get.land(Options)

Draw.temperature<-function(temperature,Options,Trange=10) {
  
  Options.local<-Options
  Options.local$fog.min.transparency<-0.5
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



Draw.pressure<-function(mslp,Options,colour=c(0,0,0)) {

  M<-GSDF.WeatherMap:::WeatherMap.rotate.pole(mslp,Options)
  lats<-M$dimensions[[GSDF.find.dimension(M,'lat')]]$values
  longs<-M$dimensions[[GSDF.find.dimension(M,'lon')]]$values
    # Need particular data format for contourLines
  if(lats[2]<lats[1] || longs[2]<longs[1] || max(longs) > 180 ) {
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
         gp<-gpar(col=rgb(colour[1],colour[2],colour[3],tp),
                             lwd=Options$mslp.lwd*lwd,lty=lt)
         # xspline won't draw very long lines
         x<-split(lines[[i]]$x,ceiling(seq_along(lines[[i]]$x)/500))
         y<-split(lines[[i]]$y,ceiling(seq_along(lines[[i]]$y)/500))
         for(s in seq_along(x)) {
            if(length(x[[s]])<2) next
            grid.xspline(x=unit(x[[s]],'native'),
                         y=unit(y[[s]],'native'),
                         shape=1,
                         gp=gp)
         }
     }
  }
}

plot.hour<-function(year,month,day,hour) {    

    image.name<-sprintf("%04d-%02d-%02d:%02d.png",year,month,day,hour)

    ifile.name<-sprintf("%s/%s",Imagedir,image.name)
    if(file.exists(ifile.name) && file.info(ifile.name)$size>0) return()

    prmsl<-MERRA.get.slice.at.hour('SLP',year,month,day,hour)
    prmsl.normal<-MERRA.get.slice.at.hour('SLP',year,month,day,hour,
                                             type='normal')
    t2m<-MERRA.get.slice.at.hour('T2M',year,month,day,hour)
    t2m.normal<-MERRA.get.slice.at.hour('T2M',year,month,day,hour,
                                             type='normal')
    t2m.sd<-MERRA.get.slice.at.hour('T2M',year,month,day,hour,
                                             type='standard.deviation')
    prmsl$data[]<-prmsl$data-prmsl.normal$data
    t2m$data[]<-(t2m$data-t2m.normal$data)/t2m.sd$data
    icec<-MERRA.get.slice.at.hour('FRSEAICE',year,month,day,hour)
    prate<-MERRA.get.slice.at.hour('PRECTOT',year,month,day,hour)

     png(ifile.name,
             width=1080*aspect,
             height=1080,
             bg=Options$sea.colour,
             pointsize=24,
             type='cairo')
       pushViewport(dataViewport(c(Options$lon.min,Options$lon.max),
                                 c(Options$lat.min,Options$lat.max),
                                  extension=0))
          ip<-WeatherMap.rectpoints(Options$ice.points,Options)
          WeatherMap.draw.ice(ip$lat,ip$lon,icec,Options)
          WeatherMap.draw.land(land,Options)

        Draw.temperature(t2m,Options,Trange=3)
        Draw.pressure(prmsl,Options,colour=c(0.3,0.3,0.3))
        WeatherMap.draw.precipitation(prate,Options)

        Options$label<-sprintf("%04d-%02d-%02d:%02d",year,month,day,hour)
        WeatherMap.draw.label(Options)
     upViewport()

    dev.off()
}

for(n.count in seq(0,n.total)) {

    n.date<-c.date+n.count/24
    year<-as.numeric(as.character(years(n.date)))
    month<-months(n.date)
    day<-days(n.date)
    hour<-n.count%%24

    image.name<-sprintf("%04d-%02d-%02d:%02d.png",year,month,day,hour)
    ifile.name<-sprintf("%s/%s",Imagedir,image.name)
    if(file.exists(ifile.name) && file.info(ifile.name)$size>0) next
    # Each plot in a seperate parallel process
    #mcparallel(plot.hour(year,month,day,hour))
    plot.hour(year,month,day,hour)
    if(hour%%8==0) mccollect(wait=TRUE)

}
mccollect()
