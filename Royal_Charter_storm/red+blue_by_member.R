#!/usr/common/software/R/3.2.2/hsw/intel/bin/Rscript --no-save

# Pressures and temperatures from 20CR2c for 1859-10-26 - royal charter storm

# Compares 4 different ensemble members

library(GSDF.TWCR)
library(GSDF.WeatherMap)
library(parallel)
library(chron)

Year<-1859
Month<-10
Day<-20
Hour<-0.23
n.total<-0#7*24*6
version<-'3.5.1'
fog.threshold<-exp(1)

GSDF.cache.dir<-sprintf("%s/GSDF.cache",Sys.getenv('SCRATCH'))
if(!file.exists(GSDF.cache.dir)) dir.create(GSDF.cache.dir,recursive=TRUE)
Imagedir<-sprintf("%s/images/Royal_Charter_rbbm",Sys.getenv('SCRATCH'))
if(!file.exists(Imagedir)) dir.create(Imagedir,recursive=TRUE)

c.date<-chron(dates=sprintf("%04d/%02d/%02d",Year,Month,Day),
            times=sprintf("%02d:00:00",Hour),
            format=c(dates='y/m/d',times='h:m:s'))

Options<-WeatherMap.set.option(NULL)

Options<-WeatherMap.set.option(NULL)
Options<-WeatherMap.set.option(Options,'show.mslp',T)
Options<-WeatherMap.set.option(Options,'show.ice',T)
Options<-WeatherMap.set.option(Options,'show.obs',T)
Options<-WeatherMap.set.option(Options,'show.fog',T)
Options<-WeatherMap.set.option(Options,'show.precipitation',T)
Options<-WeatherMap.set.option(Options,'temperature.range',12)
Options<-WeatherMap.set.option(Options,'obs.size',0.5)
Options<-WeatherMap.set.option(Options,'obs.colour',rgb(255,215,0,255,
+                                                        maxColorValue=255))
Options<-WeatherMap.set.option(Options,'lat.min',-30)
Options<-WeatherMap.set.option(Options,'lat.max',30)
Options<-WeatherMap.set.option(Options,'lon.min',-40)
Options<-WeatherMap.set.option(Options,'lon.max',40)
Options<-WeatherMap.set.option(Options,'pole.lon',160)
Options<-WeatherMap.set.option(Options,'pole.lat',35)

Options$precip.points<-100000
Options$mslp.base=0                    # Base value for anomalies
Options$mslp.range=50000                    # Anomaly for max contour
Options$mslp.step=500                       # Smaller -more contours
Options$mslp.tpscale=500                    # Smaller -contours less transparent
Options$mslp.lwd=1
land<-WeatherMap.get.land(Options)

get.member.at.hour<-function(variable,year,month,day,hour,member) {
  
       t<-TWCR.get.members.slice.at.hour(variable,year,month,day,
                                  hour,version='3.5.1')
       t<-GSDF.select.from.1d(t,'ensemble',member)
       return(t)
  }

Draw.temperature<-function(temperature,Options,Trange=10) {
    
    Options.local<-Options
    Options.local$fog.min.transparency<-0.4
    tplus<-temperature
    tplus$data[]<-pmax(1,pmin(Trange,tplus$data))/Trange
    Options.local$fog.colour<-c(1,0,0)
    WeatherMap.draw.fog(tplus,Options.local)
    tminus<-temperature
    tminus$data[]<-tminus$data*-1
    tminus$data[]<-pmax(1,pmin(Trange,tminus$data))/Trange
    Options.local$fog.colour<-c(0,0,1)
    WeatherMap.draw.fog(tminus,Options.local)
  }


Draw.pressure<-function(mslp,Options,colour=c(0,0,0)) {
  
    M<-GSDF.WeatherMap:::WeatherMap.rotate.pole(mslp,Options)
    lats<-M$dimensions[[GSDF.find.dimension(M,'lat')]]$values
    longs<-M$dimensions[[GSDF.find.dimension(M,'lon')]]$values
      # Need particular data format for contourLines
    if(lats[2]<lats[1] || longs[2]<longs[1] || max(longs) 180 ) {
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
           grid.xspline(x=unit(lines[[i]]$x,'native'),
                      y=unit(lines[[i]]$y,'native'),
                      shape=1,
                      gp=gp)
       }
    }
  }

plot.hour<-function(year,month,day,hour) {    
  
      image.name<-sprintf("%04d-%02d-%02d:%02d:%02d.png",year,month,day,as.integer(hour),
                                                         as.integer(hour%%1*60))
  
      ifile.name<-sprintf("%s/%s",Imagedir,image.name)
      if(file.exists(ifile.name) && file.info(ifile.name)$size>0) return()
  
      t2m.normal<-TWCR.get.slice.at.hour('air.2m',year,month,day,hour,version='3.5.4',
                                               type='normal')
      #prmsl.sd<-TWCR.get.slice.at.hour('prmsl',year,month,day,hour,
      #                                     version='3.4.1',type='standard.deviation')
      prmsl.normal<-TWCR.get.slice.at.hour('prmsl',year,month,day,hour,version='3.4.1',
                                               type='normal')
         obs<-TWCR.get.obs(year,month,day,hour,version='3.5.1')
         w<-which(obs$Longitude>180)
         obs$Longitude[w]<-obs$Longitude[w]-360
  
      members<-c(1,2,3,4)
      x.min<-c(0.02,0.51,0.02,0.51)
      y.min<-c(0.05,0.05,0.525,0.525)
  
       png(ifile.name,
               width=1080*WeatherMap.aspect(Options),
               height=1080,
               bg='white',
               pointsize=24,
               type='cairo')
  
      for(vn in seq(1,4)) {
  
  	prmsl<-get.member.at.hour('prmsl',year,month,day,hour,members[vn])
  	#prmsl.spread<-TWCR.get.slice.at.hour('prmsl',year,month,day,hour,version=versions[vn],
  	#					  type='spread')
  	#fog<-TWCR.relative.entropy(prmsl.normal,prmsl.sd,prmsl,prmsl.spread)
  	#fog$data[]<-1-pmin(fog.threshold,pmax(0,fog$data))/fog.threshold
  	t2m<-get.member.at.hour('air.2m',year,month,day,hour,members[vn])
  	prate<-get.member.at.hour('prate',year,month,day,hour,members[vn])
  	prmsl$data[]<-as.vector(prmsl$data)-as.vector(prmsl.normal$data)
  	t2m$data[]<-as.vector(t2m$data)-as.vector(t2m.normal$data)
  
  	   pushViewport(viewport(width=0.47,height=0.45,x=x.min[vn],y=y.min[vn],
  			      just=c("left","bottom"),clip='on'))
  
  	   pushViewport(dataViewport(c(Options$lon.min,Options$lon.max),
  				     c(Options$lat.min,Options$lat.max),
  				      extension=0))
                grid.polygon(x=unit(c(0,1,1,0),'npc'),y=unit(c(0,0,1,1),'npc'),
                             gp=gpar(col=Options$sea.colour,fill=Options$sea.colour))
  	      ip<-WeatherMap.rectpoints(Options$ice.points,Options)
  	      #WeatherMap.draw.ice(ip$lat,ip$lon,icec,Options)
  	      WeatherMap.draw.land(land,Options)
  
  	    Draw.temperature(t2m,Options)
  	    WeatherMap.draw.obs(obs,Options)
  	    Draw.pressure(prmsl,Options,colour=c(0,0,0))
  	    WeatherMap.draw.precipitation(prate,Options)
  	    #WeatherMap.draw.fog(fog,Options)
  
  	    #Options$label<-captions[vn]
  	    #WeatherMap.draw.label(Options)
  	 upViewport()
  	 upViewport()
  
      }
  	 Options$label<-sprintf("%04d-%02d-%02d:%02d",year,month,day,hour)
  	 #WeatherMap.draw.label(Options)
           grid.text(Options$label,y=unit(0.01,'npc'),x=unit(0.99,'npc'),
                     just=c('right','bottom'))
  
      dev.off()
  }

for(n.count in seq(0,n.total)) {
  
      n.date<-c.date+n.count/(24*6) # 10-minute timesteps
      year<-as.numeric(as.character(years(n.date)))
      month<-months(n.date)
      day<-days(n.date)
      hour<-hours(n.date)+minutes(n.date)/60
  
      image.name<-sprintf("%04d-%02d-%02d:%02d:%02d.png",year,month,day,as.integer(hour),
                                                         as.integer(hour%%1*60))
      ifile.name<-sprintf("%s/%s",Imagedir,image.name)
      if(file.exists(ifile.name) && file.info(ifile.name)$size>0) next
      # Each plot in a seperate parallel process
      mcparallel(plot.hour(year,month,day,hour))
      if(n.count%%20==0) mccollect(wait=TRUE)
  
  }
