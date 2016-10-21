#!/usr/bin/Rscript --no-save

# Voronoi polyhedra.
# Flat version - with subtle colours for background
# With added colour

library(GSDF.TWCR)
library(GSDF.WeatherMap)
library(grid)
library(deldir)
library(RColorBrewer)
library(chron)

Year<-2014
Month<-1
Day<-1
Hour<-1
n.total<-200#365*24
version<-'3.5.1'
cores<-24

c.date<-chron(dates=sprintf("%04d/%02d/%02d",Year,Month,Day),
          times=sprintf("%02d:00:00",Hour),
          format=c(dates='y/m/d',times='h:m:s'))


GSDF.cache.dir<-sprintf("%s/GSDF.cache",Sys.getenv('SCRATCH'))
if(!file.exists(GSDF.cache.dir)) dir.create(GSDF.cache.dir,recursive=TRUE)
local.cache.dir<-sprintf("%s/images/TWCR_voronoi_subtle",Sys.getenv('SCRATCH'))
if(!file.exists(local.cache.dir)) dir.create(local.cache.dir,recursive=TRUE)
Imagedir<-sprintf("%s/images/TWCR_voronoi_full",Sys.getenv('SCRATCH'))
if(!file.exists(Imagedir)) dir.create(Imagedir,recursive=TRUE)

Options<-WeatherMap.set.option(NULL)
Options<-WeatherMap.set.option(Options,'land.colour',rgb(75,75,75,255,
                                                       maxColorValue=255))
Options<-WeatherMap.set.option(Options,'sea.colour',rgb(150,150,150,255,
                                                       maxColorValue=255))
Options<-WeatherMap.set.option(Options,'ice.colour',rgb(250,250,250,255,
                                                       maxColorValue=255))
Options<-WeatherMap.set.option(Options,'background.resolution','high')
Options<-WeatherMap.set.option(Options,'pole.lon',160)
Options<-WeatherMap.set.option(Options,'pole.lat',35)

Options<-WeatherMap.set.option(Options,'lat.min',-90)
Options<-WeatherMap.set.option(Options,'lat.max',90)
Options<-WeatherMap.set.option(Options,'lon.min',-190)
Options<-WeatherMap.set.option(Options,'lon.max',190)
Options$vp.lon.min<- -180
Options$vp.lon.max<-  180
Options<-WeatherMap.set.option(Options,'wrap.spherical',F)

Options<-WeatherMap.set.option(Options,'wind.vector.points',3)
Options<-WeatherMap.set.option(Options,'wind.vector.scale',0.1)
Options<-WeatherMap.set.option(Options,'wind.vector.move.scale',1)
Options<-WeatherMap.set.option(Options,'wind.vector.density',1)
Options$ice.points<-100000

cols <- brewer.pal(9,"Greys")
col<-rgb(113,113,113,maxColorValue=255)
alphas<-seq(0.025,0.175,0.005)

Options$mslp.base=101325                    # Base value for anomalies
Options$mslp.range=50000                    # Anomaly for max contour
Options$mslp.step=500                       # Smaller -> more contours
Options$mslp.tpscale=500                    # Smaller -> contours less transparent
Options$mslp.lwd=2
Options$precip.colour=c(0,0.2,0)

Draw.temperature<-function(temperature,Options,Trange=1) {

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
  M<-GSDF:::GSDF.pad.longitude(M) # Extras for periodic boundary conditions
  lats<-M$dimensions[[GSDF.find.dimension(M,'lat')]]$values
  longs<-M$dimensions[[GSDF.find.dimension(M,'lon')]]$values
    # Need particular data format for contourLines
  if(lats[2]<lats[1] || longs[2]<longs[1] || max(longs) > 182 ) {
    if(lats[2]<lats[1]) lats<-rev(lats)
    if(longs[2]<longs[1]) longs<-rev(longs)
    longs[longs>182]<-longs[longs>182]-364
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
         grid.xspline(x=unit(lines[[i]]$x,'native'),
                    y=unit(lines[[i]]$y,'native'),
                    shape=1,
                    gp=gp)
     }
  }
}

make.streamlines<-function(year,month,day,hour,streamlines=NULL) {


    sf.name<-sprintf("%s/streamlines.%04d-%02d-%02d:%02d.rd",
                           local.cache.dir,year,month,day,hour)
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
    save(year,month,day,hour,s,file=sf.name)
    gc(verbose=FALSE)
    return(s)

}

# Move vertices of a new tile towards its origin point
shrink.new.tiles<-function(vtess,s) {
  weight<-c(.1,.4,.7)
  for(st in c(1,2,3)) {
    w<-which(s$status==st) # new points
    w2<-s$id[w]            # persistent ids of new points
    w3<-which(vtess$summary$z %in%w2) # tiles containing points with these ids
    for(tile in w3) {
      w4<-which(vtess$dirsgs$ind1==tile) # vertices of this tile
      vtess$dirsgs$x1[w4]<-vtess$dirsgs$x1[w4]*weight[st]+
                           vtess$summary$x[tile]*(1-weight[st])
      vtess$dirsgs$x2[w4]<-vtess$dirsgs$x2[w4]*weight[st]+
                           vtess$summary$x[tile]*(1-weight[st])
      vtess$dirsgs$y1[w4]<-vtess$dirsgs$y1[w4]*weight[st]+
                           vtess$summary$y[tile]*(1-weight[st])
      vtess$dirsgs$y2[w4]<-vtess$dirsgs$y2[w4]*weight[st]+
                           vtess$summary$y[tile]*(1-weight[st])
    }
  }
  return(vtess)
}
  

plot.hour<-function(year,month,day,hour,streamlines) {


    image.name<-sprintf("%04d-%02d-%02d:%02d.png",year,month,day,hour)

    ifile.name<-sprintf("%s/%s",Imagedir,image.name)
    if(file.exists(ifile.name) && file.info(ifile.name)$size>0) return()

    land<-WeatherMap.get.land(Options)

    # Store the polyhedra - they are expensive to recreate
    vf.name<-sprintf("%s/voronoi.%04d-%02d-%02d:%02d.rd",
                           local.cache.dir,year,month,day,hour)
    if(file.exists(vf.name) && file.info(vf.name)$size>5000) {
       load(vf.name)
    } else {
       vtess<-deldir(s$x[,1],s$y[,1],z=s$id)
       vtess<-shrink.new.tiles(vtess,s)
       tl<-tile.list(vtess)
       save(vtess,tl,file=vf.name)
     }
    
    t2m<-TWCR.get.slice.at.hour('air.2m',year,month,day,hour,version=version)
    t2m.normal<-TWCR.get.slice.at.hour('air.2m',year,month,day,hour,
                                             type='normal',version='3.4.1')
    t2m$data[]<-t2m$data-t2m.normal$data
    prmsl.T<-TWCR.get.slice.at.hour('prmsl',year,month,day,hour,version=version)
    icec<-TWCR.get.slice.at.hour('icec',year,month,day,hour,version=version)
    prate<-TWCR.get.slice.at.hour('prate',year,month,day,hour,version=version)    
     png(ifile.name,
             width=1050*2,
             height=1050,
             bg=Options$sea.colour,
             pointsize=24,
             type='cairo')
  base.gp<-gpar(family='Helvetica',font=1,col='black')
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
      WeatherMap.draw.land(land,Options)
#      Draw.pressure(prmsl.T,Options,colour=c(0.91,0.46,0))
       Draw.temperature(t2m,Options,Trange=10)
       WeatherMap.draw.precipitation(prate,Options)
    Draw.pressure(prmsl.T,Options,colour=c(0,0,0))
    for(p in seq_along(tl)) {
      #col<-cols[tl[[p]]$z%%length(cols)+1]
      
      gp<-gpar(col=cols,fill=col,
               alpha=alphas[tl[[p]]$z%%length(alphas)+1],
               lwd=0)
      grid.polygon(x=unit(tl[[p]]$x,'native'),
                   y=unit(tl[[p]]$y,'native'),
                   gp=gp)
    }
 
    dev.off()
}

s<-NULL
for(n.count in seq(0,n.total)) {

    n.date<-c.date+n.count/24
    year<-as.numeric(as.character(years(n.date)))
    month<-months(n.date)
    day<-days(n.date)
    hour<-(n.count+Hour)%%24

    # serial component - streamlines evolve from hour to hour
    s<-make.streamlines(year,month,day,hour,streamlines=s)

    image.name<-sprintf("%04d-%02d-%02d:%02d.png",year,month,day,hour)
    ifile.name<-sprintf("%s/%s",Imagedir,image.name)
    if(file.exists(ifile.name) && file.info(ifile.name)$size>0) next

    # Each plot in a seperate parallel process
    #plot.hour(year,month,day,hour,s)
    mcparallel(plot.hour(year,month,day,hour,s))
    if(n.count%%cores==0) mccollect(wait=TRUE)

}
mccollect()
