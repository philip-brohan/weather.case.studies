# Plot an example of the precip shockwave effect in ERA5

library(grid)
library(GSDF.ERA5)
library(RColorBrewer)

year<-2016
month<-1
day<-4

cols<-colorRampPalette(brewer.pal(9,"Greys"))(100)

plot.region<-function(year,month,day,hour,fc.init,
                      lat.min,lat.max,lon.min,lon.max) {
     pushViewport(plotViewport(margins=c(4,3,1,1)))
     pushViewport(dataViewport(c(lon.min,lon.max),
                               c(lat.min,lat.max),clip='off'))
     grid.xaxis(main=T)
     grid.text(sprintf("%04d-%02d-%02d:%02d (%02d)",year,
	                         month,day,hour,fc.init),
				 y=unit(-3,'lines'))
     grid.yaxis(main=T)
   field<-ERA5.get.slice.at.hour('prate',year,month,day,hour,fc.init=fc.init)
   field$data[]<-sqrt(field$data)
	     
  for(lon in seq_along(field$dimensions[[1]]$values)) {

      x<-field$dimensions[[1]]$values[lon]
      dx<-(field$dimensions[[1]]$values[2]-
          field$dimensions[[1]]$values[1])*0.9
      p.x<-c(x-dx/2,x+dx/2,x+dx/2,x-dx/2)
      if(min(p.x)<lon.min || max(p.x)>lon.max) next

     for(lat in seq_along(field$dimensions[[2]]$values)) {

      y<-field$dimensions[[2]]$values[lat]
      dy<-(field$dimensions[[2]]$values[2]-
          field$dimensions[[2]]$values[1])*0.9
      p.y<-c(y-dy/2,y-dy/2,y+dy/2,y+dy/2)
      if(min(p.y)<lat.min || max(p.y)>lat.max) next

      col.i<-min(100,max(1,field$data[lon,lat,1]/0.001))
      gp=gpar(col=cols[col.i],fill=cols[col.i])
      #gp=gpar(col=rgb(0,0,0,1),fill=rgb(0,0,0,1))
	     
      grid.polygon(x=unit(p.x,'native'),
                   y=unit(p.y,'native'),
                   gp=gp)
   }
 }
			       
   popViewport()
   popViewport()
}

pdf('precip.shockwave.pdf',
      width=24*3,
      height=6,
      pointsize=16)

for(hour in seq(0,23)) {

   pushViewport(viewport(width=1/24,height=0.5,x=(hour-0)/24,y=0.5,
                         just=c("left","bottom")))
   if(hour<12 || hour> 18) {
      plot.region(year,month,day,hour,18,-15,5,80,100)
   }
   popViewport()
   pushViewport(viewport(width=1/24,height=0.5,x=(hour-0)/24,y=0,
                         just=c("left","bottom")))
   if(hour>6) {
      plot.region(year,month,day,hour,6,-15,5,80,100)
   }
   popViewport()

}
dev.off()