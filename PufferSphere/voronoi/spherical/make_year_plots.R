#!/usr/bin/Rscript --no-save

# Voronoi polyhedra.
# Spherical version

library(GSDF.TWCR)
library(GSDF.WeatherMap)
library(grid)
library(deldir)
library(RColorBrewer)
library(chron)

Year<-2014
Month<-1
Day<-1
Hour<-0
n.total<-1000#365*24
version<-'3.5.1'
cores<-16

c.date<-chron(dates=sprintf("%04d/%02d/%02d",Year,Month,Day),
          times=sprintf("%02d:00:00",Hour),
          format=c(dates='y/m/d',times='h:m:s'))


GSDF.cache.dir<-sprintf("%s/GSDF.cache",Sys.getenv('SCRATCH'))
if(!file.exists(GSDF.cache.dir)) dir.create(GSDF.cache.dir,recursive=TRUE)
Imagedir<-sprintf("%s/images/TWCR_voronoi_spherical",Sys.getenv('SCRATCH'))
if(!file.exists(Imagedir)) dir.create(Imagedir,recursive=TRUE)

Options<-WeatherMap.set.option(NULL)
Options<-WeatherMap.set.option(Options,'lat.min',-90)
Options<-WeatherMap.set.option(Options,'lat.max',90)
Options<-WeatherMap.set.option(Options,'lon.min',-190)
Options<-WeatherMap.set.option(Options,'lon.max',190)
Options$vp.lon.min<- -180
Options$vp.lon.max<-  180
Options<-WeatherMap.set.option(Options,'wrap.spherical',TRUE)
Options<-WeatherMap.set.option(Options,'pole.lon',160)
Options<-WeatherMap.set.option(Options,'pole.lat',35)

Options<-WeatherMap.set.option(Options,'wind.vector.points',3)
Options<-WeatherMap.set.option(Options,'wind.vector.scale',0.1)
Options<-WeatherMap.set.option(Options,'wind.vector.move.scale',1)
Options<-WeatherMap.set.option(Options,'wind.vector.density',2)
Options<-WeatherMap.set.option(Options,'cores',1)

cols <- brewer.pal(9,"Set1")

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

make.streamlines<-function(year,month,day,hour,streamlines=NULL) {

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

    vtess<-deldir(s$x[,1],s$y[,1],z=s$id)
    vtess<-shrink.new.tiles(vtess,s)
    tl<-tile.list(vtess)
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
    for(p in seq_along(tl)) {
      col<-cols[tl[[p]]$z%%length(cols)+1]
      
      if(Options$wrap.spherical & (max(tl[[p]]$y)>89 | min(tl[[p]]$y)< -89)) next
      gp<-gpar(col=col,fill=col)
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
