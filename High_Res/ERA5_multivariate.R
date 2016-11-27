#!/usr/bin/Rscript --no-save

# Wind, ice, pressure, temperature and precip.
# ERA5 data, print quality


library(GSDF.ERA5)
library(GSDF.TWCR)
library(GSDF.WeatherMap)
library(grid)
#library(RColorBrewer)
#library(colorspace)
library(extrafont)
#loadfonts()

opt = list(
  year = 2016,
  month = 1,
  day = 30,
  hour = 12
  )

Imagedir<-sprintf(".",Sys.getenv('SCRATCH'))

Options<-WeatherMap.set.option(NULL)
Options<-WeatherMap.set.option(Options,'land.colour',rgb(50,50,50,255,
                                                       maxColorValue=255))
Options<-WeatherMap.set.option(Options,'sea.colour',rgb(200,200,200,255,
                                                       maxColorValue=255))
Options<-WeatherMap.set.option(Options,'ice.colour',rgb(250,250,250,255,
                                                       maxColorValue=255))
Options<-WeatherMap.set.option(Options,'background.resolution','high')
Options<-WeatherMap.set.option(Options,'pole.lon',160)
Options<-WeatherMap.set.option(Options,'pole.lat',45)

Options<-WeatherMap.set.option(Options,'lat.min',-90)
Options<-WeatherMap.set.option(Options,'lat.max',90)
Options<-WeatherMap.set.option(Options,'lon.min',-190+50)
Options<-WeatherMap.set.option(Options,'lon.max',190+50)
Options$vp.lon.min<- -180+50
Options$vp.lon.max<-  180+50
Options<-WeatherMap.set.option(Options,'wrap.spherical',F)

Options<-WeatherMap.set.option(Options,'wind.vector.points',3)
Options<-WeatherMap.set.option(Options,'wind.vector.scale',0.2)
Options<-WeatherMap.set.option(Options,'wind.vector.move.scale',1)
Options<-WeatherMap.set.option(Options,'wind.vector.density',0.5)
Options<-WeatherMap.set.option(Options,'wind.vector.lwd',1.5)
Options$ice.points<-100000
Options<-WeatherMap.set.option(Options,'precip.min.transparency',0.9)
Options<-WeatherMap.set.option(Options,'fog.min.transparency',0.0)

Options$mslp.base=0                         # Base value for anomalies
Options$mslp.range=50000                    # Anomaly for max contour
Options$mslp.step=500                       # Smaller -> more contours
Options$mslp.tpscale=500                    # Smaller -> contours less transparent
Options$mslp.lwd=2
Options$precip.colour=c(0,0.2,0)


WeatherMap.streamline.getGC<-function(value,transparency=NA,status=1,Options) {
   status=4
   alpha<-c(10,50,150,255)[min(status,4)]
   return(gpar(col=rgb(125,125,125,alpha,maxColorValue=255),
               fill=rgb(125,125,125,alpha,maxColorValue=255),lwd=Options$wind.vector.lwd))
}
assignInNamespace("WeatherMap.streamline.getGC",WeatherMap.streamline.getGC, ns="GSDF.WeatherMap")

lsm<-GSDF.ncdf.load(sprintf("%s/ERA5/oper/fixed_land_mask.nc",Sys.getenv('SCRATCH')),'lsm',
                             lat.range=c(-90,90),lon.range=c(0,360))
orog<-GSDF.ncdf.load(sprintf("%s/ERA5/oper/fixed_geopotential.nc",Sys.getenv('SCRATCH')),'z',
                             lat.range=c(-90,90),lon.range=c(0,360))
w<-which(lsm$data==0)
is.na(orog$data[w])<-TRUE

draw.land<-function(Options,n.levels=20) {
   land<-GSDF.WeatherMap:::WeatherMap.rotate.pole(GSDF:::GSDF.pad.longitude(orog),Options)
   lons<-land$dimensions[[GSDF.find.dimension(land,'lon')]]$values
   qtls<-quantile(land$data,probs=seq(0,1,1/n.levels),na.rm=TRUE)
   base.colour<-col2rgb(Options$land.colour)/255
   peak.colour<-c(.8,.8,.8)
   for(level in seq_along(qtls)) {
      if(level==1) next
      fraction<-1-(level-1)/n.levels
      plot.colour<-base.colour*fraction+peak.colour*(1-fraction)
      plot.colour<-rgb(plot.colour[1],plot.colour[2],plot.colour[3])
      plot.colours<-rep(rgb(0,0,0,0),length(land$data))
      w<-which(land$data>=qtls[level-1] & land$data<=qtls[level])
      plot.colours[w]<-plot.colour
      m<-matrix(plot.colours, ncol=length(lons), byrow=T)
      # flip the data order up<->down to be right for an image
      #m<-apply(m,2,rev)
      r.w<-max(lons)-min(lons)+(lons[2]-lons[1])
      r.c<-(max(lons)+min(lons))/2
      grid.raster(m,,
                   x=unit(r.c,'native'),
                   y=unit(0,'native'),
                   width=unit(r.w,'native'),
                   height=unit(180,'native'))
   }   
}
 
Draw.temperature<-function(temperature,Options,Trange=1) {

  Options.local<-Options
  Options.local$fog.min.transparency<-0.8
  Options.local$fog.resolution<-0.25
  tplus<-temperature
  tplus$data[]<-pmax(0,pmin(Trange,tplus$data))
  tplus$data[]<-sqrt(tplus$data[])/Trange
  Options.local$fog.colour<-c(1,0,0)
  WeatherMap.draw.fog(tplus,Options.local)
  tminus<-temperature
  tminus$data[]<-tminus$data*-1
  tminus$data[]<-pmax(0,pmin(Trange,tminus$data))
  tminus$data[]<-sqrt(tminus$data[])/Trange
  Options.local$fog.colour<-c(0,0,1)
  WeatherMap.draw.fog(tminus,Options.local)
}

Draw.pressure<-function(mslp,Options,colour=c(0,0,0)) {

  M<-GSDF.WeatherMap:::WeatherMap.rotate.pole(mslp,Options)
  M<-GSDF:::GSDF.pad.longitude(M) # Extras for periodic boundary conditions
  lats<-M$dimensions[[GSDF.find.dimension(M,'lat')]]$values
  longs<-M$dimensions[[GSDF.find.dimension(M,'lon')]]$values
    # Need particular data format for contourLines
  maxl<-Options$vp.lon.max+2
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
             lwd<-0.5
         }
         gp<-gpar(col=rgb(colour[1],colour[2],colour[3],tp),
                             lwd=Options$mslp.lwd*lwd,lty=lt)
         res<-tryCatch({
	     grid.xspline(x=unit(lines[[i]]$x,'native'),
			y=unit(lines[[i]]$y,'native'),
			shape=1,
			gp=gp)
             }, warning = function(w) {
                 print(w)
             }, error = function(e) {
                print(e)
             }, finally = {
                # Do nothing
             })
     }
  }
}

get.streamlines<-function(year,month,day,hour) {


    sf.name<-sprintf("%s/streamlines.%04d-%02d-%02d:%02d.rd",
                           Imagedir,year,month,day,hour)
    if(file.exists(sf.name) && file.info(sf.name)$size>5000) {
       load(sf.name)
       return(s)
    } else {
      stop(sprintf("No streamlines available for %04d-%02d-%02d:%02d",
                   year,month,day,hour))
    }

}

plot.hour<-function(year,month,day,hour,streamlines) {

    image.name<-sprintf("%04d-%02d-%02d:%02d.pdf",year,month,day,hour)

    ifile.name<-sprintf("%s/%s",Imagedir,image.name)

    #land<-WeatherMap.get.land(Options)
    
    t2m<-ERA5.get.slice.at.hour('air.2m',year,month,day,hour)
    t2n<-readRDS(sprintf("%s/ERA5/oper/climtologies.test/air.2m.%02d.Rdata",
                           Sys.getenv('SCRATCH'),hour))
    t2m$data[]<-t2m$data-t2n$data
    prmsl.T<-ERA5.get.slice.at.hour('prmsl',year,month,day,hour)
    prn<-readRDS(sprintf("%s/ERA5/oper/climtologies.test/prmsl.%02d.Rdata",
                           Sys.getenv('SCRATCH'),hour))
    prmsl.T$data[]<-prmsl.T$data-prn$data
    icec<-ERA5.get.slice.at.hour('icec',year,month,day,hour)
    prate<-ERA5.get.slice.at.hour('prate',year,month,day,hour)
    prate$data[]<-prate$data/3
     pdf(ifile.name,
             width=33.1,
             height=23.4,
             bg=Options$sea.colour,
	     family='Helvetica',
             pointsize=24)
  base.gp<-gpar(fontfamily='Helvetica',fontface='bold',col='black')
  lon.min<-Options$lon.min
  if(!is.null(Options$vp.lon.min)) lon.min<-Options$vp.lon.min
  lon.max<-Options$lon.max
  if(!is.null(Options$vp.lon.max)) lon.max<-Options$vp.lon.max
  lat.min<-Options$lat.min
  if(!is.null(Options$vp.lat.min)) lat.min<-Options$vp.lat.min
  lat.max<-Options$lat.max
  if(!is.null(Options$vp.lat.max)) lat.max<-Options$vp.lat.max
  pushViewport(dataViewport(c(lon.min,lon.max),c(lat.min,lat.max),
		            extension=0,gp=base.gp))
    
      ip<-WeatherMap.rectpoints(Options$ice.points,Options)
      WeatherMap.draw.ice(ip$lat,ip$lon,icec,Options)
      draw.land(Options,n.levels=100)
      WeatherMap.draw.streamlines(streamlines,Options)
       Draw.temperature(t2m,Options,Trange=10)
       WeatherMap.draw.precipitation(prate,Options)
    Draw.pressure(prmsl.T,Options,colour=c(0,0,0))
    Options$label=sprintf("%04d/%02d/%02d:%02d",year,month,day,hour)
    Options<-WeatherMap.set.option(Options,'land.colour',Options$sea.colour)
    WeatherMap.draw.label(Options)
    dev.off()
}

# Use pre-calculated streamlines
s<-get.streamlines(opt$year,opt$month,opt$day,opt$hour)
plot.hour(opt$year,opt$month,opt$day,opt$hour,s)
image.name<-sprintf("%04d-%02d-%02d:%02d.pdf",
                    opt$year,opt$month,opt$day,opt$hour)
ifile.name<-sprintf("%s/%s",Imagedir,image.name)
embed_fonts(ifile.name)
