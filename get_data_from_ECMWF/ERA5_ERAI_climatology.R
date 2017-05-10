#!/usr/bin/env Rscript
# Make not-quite-so-poor-man's climatologies for each hour from the ERA5 data
#  by adjusting the ERA Interim climatologies

library(GSDF.ERA5)
library(GSDF.ERAI)
library(lubridate)
library(getopt)

opt = getopt(c(
  'hour',   'h', 2, "integer"
))
 
var<-'air.2m'

hour<-opt$hour

   print(hour)
   c.date<-ymd("2016-01-01")
   c<-ERA5.get.slice.at.hour(var,lubridate::year(c.date),
                                  lubridate::month(c.date),
                                  lubridate::day(c.date),
                                  hour)
   i<-ERAI.get.slice.at.hour(var,lubridate::year(c.date),
                                  lubridate::month(c.date),
                                  lubridate::day(c.date),
                                  hour)
   i<-GSDF.regrid.2d(i,c)
   c$data[]<-c$data-i$data
   count<-1
   while(c.date<ymd("2016-02-28")) {
     c.date<-c.date+lubridate::days(1)
     d<-ERA5.get.slice.at.hour(var,lubridate::year(c.date),
                                   lubridate::month(c.date),
                                   lubridate::day(c.date),
                                   hour)
     i<-ERAI.get.slice.at.hour(var,lubridate::year(c.date),
                                   lubridate::month(c.date),
                                   lubridate::day(c.date),
                                   hour)
     i<-GSDF.regrid.2d(i,c)
     d$data[]<-d$data-i$data
     c$data[]<-c$data+d$data
     count<-count+1
   }
   c$data[]<-c$data/count
   for(month in seq(1,12)) {
     dim<-days_in_month(ymd(sprintf("%04d-%02d-10",1981,month)))
     for(day in seq(1,dim)) {
       i<-ERAI.get.slice.at.hour(var,1981,month,day,hour,type='normal')
       i<-GSDF.regrid.2d(i,c)
       i$data[]<-i$data+c$data
       saveRDS(i,sprintf("%s/ERA5/climtologies.1981-2010/%s.%02d.%02d.%02d.Rdata",
                     Sys.getenv("SCRATCH"),var,month,day,hour))
     }
   }
