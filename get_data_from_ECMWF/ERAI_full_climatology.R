#!/usr/bin/env Rscript

# Make climatologies for each day and hour from ERAI data
#  by averaging over 1981-2010.

library(GSDF.ERAI)
library(lubridate)
library(getopt)

opt = getopt(c(
  'var',    'v', 2, "character",
  'month',  'm', 2, "integer",
  'day',    'd', 2, "integer",
  'hour',   'h', 2, "integer"
))

make.clim<-function(var,month,day,hour) {
           c<-ERAI.get.slice.at.hour(var,1981,month,day,hour)
           for(year in seq(1982,2010)) {
             d<-ERAI.get.slice.at.hour(var,year,month,day,hour)
             c$data[]<-c$data+d$data
           }
           c$data[]<-c$data/30
           saveRDS(c,sprintf("%s/ERA_Interim/climtologies.1981-2010/%s.%02d.%02d.%02d.Rdata",
                             Sys.getenv("SCRATCH"),var,month,day,hour))
} 

make.clim(opt$var,opt$month,opt$day,opt$hour)
