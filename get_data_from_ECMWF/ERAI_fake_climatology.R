# Make poor-man's climatologies for each hourfrom the ERAI data
#  by averaging over every day in the test period.

library(GSDF.ERAI)
library(lubridate)

var<-'air.2m'

for(hour in seq(0,23)) {

   c.date<-ymd("2016-01-01")
   c<-ERAI.get.slice.at.hour(var,lubridate::year(c.date),
                                  lubridate::month(c.date),
                                  lubridate::day(c.date),
                                  hour)
   count<-1
   while(c.date<ymd("2016-02-28")) {
     c.date<-c.date+lubridate::days(1)
     d<-ERAI.get.slice.at.hour(var,lubridate::year(c.date),
                                   lubridate::month(c.date),
                                   lubridate::day(c.date),
                                   hour)
     c$data[]<-c$data+d$data
     count<-count+1
   }
   c$data[]<-c$data/count
   saveRDS(c,sprintf("%s/ERA_Interim/climtologies.test/%s.%02d.Rdata",
                     Sys.getenv("SCRATCH"),var,hour))
}
