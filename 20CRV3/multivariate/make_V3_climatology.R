# Make a fake V3 climatology by applying a correction to the
# V2 climatology.

library(GSDF.TWCR)
library(lubridate)

var<-'air.2m'

# Assemble a mean correction for each time of day
correction<-list()
count<-rep(0,18)
for(year in seq(1918,1918)) {
  for(month in seq(1,12)) {
    ndy<-lubridate::days_in_month(ymd(sprintf("%04d-%02d-15",year,month)))
    for(day in seq(1,ndy)) {
      for(hour in c(0,6,12,18)) {
        hr<-sprintf("%02d",hour)
        new.d<-TWCR.get.members.slice.at.hour(var,year,month,day,hour,version='4.1.8')
        new.d<-GSDF.reduce.1d(new.d,'ensemble',mean)
        old.d<-TWCR.get.slice.at.hour(var,year,month,day,hour,version='3.5.1')
        old.d<-GSDF.regrid.2d(old.d,new.d)
        new.d$data[]<-new.d$data-old.d$data
        if(month==1 && day==1) {
          correction[[hr]]<-new.d
        } else {
          correction[[hr]]$data[]<-correction[[hr]]$data+new.d$data
        }
        count[hour+1]<-count[hour+1]+1
      }
    }
  }
}
for(hour in c(0,6,12,18)) {
   hr<-sprintf("%02d",hour)
   correction[[hr]]$data[]<-correction[[hr]]$data/count[hour+1]
}
saveRDS(correction,sprintf('%s.correction.Rdata',var))
q('no')

# Apply this correction to a 3.4.1 normals file
new.normals<-TWCR.get.members.slice.at.hour(var,1918,3,12,6,version='4.1.8')
new.normals<-GSDF.select.from.1d(new.normals,'ensemble',1)
nn<-new.normals
n.hours<-365*8
new.normals$data<-array(dim=c(512,256,n.hours))
start.time<-lubridate::ymd_hms("1981-01-01:00:00:00")
for(hour in seq(0,n.hours-1,2)*3) {
  c.time<-start.time+lubridate::hours(hour*3)
  old<-TWCR.get.slice.at.hour(var,
                              as.integer(lubridate::year(c.time)),
                              as.integer(lubridate::month(c.time)),
                              as.integer(lubridate::day(c.time)),
                              as.integer(lubridate::hour(c.time)),
                              version='3.4.1',type='normal')
  old<-GSDF.regrid.2d(old,nn)
  ds.hour<-sprintf("%02d",hour%%24)
  old$data[]<-as.vector(old$data)+as.vector(correction[[ds.hour]]$data)
  new.normals$data[,,hour/3+1]<-old$data
  gc()
}
for(hour in seq(1,n.hours-2,2)*3) {
  new.normals$data[,,hour/3+1]<-new.normals$data[,,hour/3]/2
                              + new.normals$data[,,hour/3+2]/2
}
hour<-(n.hours-1)*3
new.normals$data[,,hour/3+1]<-new.normals$data[,,hour/3]/2 +
                               new.normals$data[,,1]/2
new.normals$dimensions[[3]]$values<-start.time+lubridate::hours(seq(0,n.hours-1)*3)
new.normals$dimensions[[3]]$values<-sprintf("%04d-%02d-%02d:%02d:00",
                          as.integer(lubridate::year(new.normals$dimensions[[3]]$values)),
                          as.integer(lubridate::month(new.normals$dimensions[[3]]$values)),
                          as.integer(lubridate::day(new.normals$dimensions[[3]]$values)),
                          as.integer(lubridate::hour(new.normals$dimensions[[3]]$values)))
new.normals$meta$calendar<-'gregorian'
GSDF.ncdf.write(new.normals,sprintf("%s.nc",var),name=var,
                shuffle=TRUE,compression=5)
