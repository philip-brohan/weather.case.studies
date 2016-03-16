# Script to split 20CR obs data into individual hour files

library(GSDF.TWCR)
library(lubridate)

opdir<-'/data/users/hadpb/20CR/version_3.5.1/observations.by.hour'

d.start<-ymd("20140101")

for(d.inc in seq(0,365)) {
  d.current<-d.start+days(d.inc)
  year<-year(d.current)
  month<-month(d.current)
  day<-day(d.current)
  for(hour in c(0,6,12,18)) {
    obs<-GSDF.TWCR::TWCR.get.obs.1file(year,month,day,hour,'3.5.1')
    h.date<-substr(obs$UID,1,10)
    for(hr in unique(h.date)) {
      opf<-sprintf("%s/%s.Rdata",opdir,hr)
      w<-which(h.date==hr)
      obs.hr<-obs[w,]
      save(obs.hr,file=opf)
    }
  }
}
