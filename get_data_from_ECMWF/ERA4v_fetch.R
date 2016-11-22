#!/usr/bin/env Rscript

# Pull down ERA 4v data for a month from ECMWF (to $SCRATCH)
# Needs ECMWF account and python script access setup

library(getopt)
library(lubridate)

opt = getopt(matrix(c(
  'year',         'y', 2, 'integer',
  'month',        'm', 2, 'integer',
  'stream',       's', 2, 'character'  # 'oper' or 'enda'
), byrow=TRUE, ncol=4))

if(is.null(opt$year))     stop('Year must be specified')
if(is.null(opt$month))    stop('Month must be specified')
if(is.null(opt$stream))   opt$stream<-'oper'

local.dir<-sprintf("%s/ERA4v",Sys.getenv('SCRATCH'))

# Surface data - all forecast
vars<-c('2t','10u','10v','msl','ci','sst','tp')

get.surface.forecast<-function(var) {
 fname<-tempfile()
 target.dir<-sprintf("%s/%s/hourly/%04d/%02d",local.dir,opt$stream,opt$year,opt$month)
 if(!file.exists(target.dir)) dir.create(target.dir,recursive=TRUE)
 for(start.hour in c(9,21)) {
     target.file<-(sprintf("%s/%s.%02d.nc",target.dir,var,start.hour))
     if(file.exists(target.file) && file.info(target.file)$size>0) next # already done
     sink(fname)
     cat('#!/usr/bin/env python\n')
     cat('from ecmwfapi import ECMWFDataServer\n')
     cat('server = ECMWFDataServer()\n')
     cat('server.retrieve({\n')
     cat('   \'dataset\'   : "era5_test",\n')
     cat(sprintf("   'stream'    : \"%s\",\n",opt$stream))
     cat('   \'type\'      : "4v",\n')
     cat('   \'step\'      : "0/to/18/by/1",\n')
     cat('   \'levtype\'   : "sfc",\n')
     cat(sprintf("   \'param\'     : \"%s\",\n",var))
     cat('   \'grid\'      : "0.25/0.25",\n')
     cat(sprintf("   \'time\'      : \"%02d\",\n",start.hour))
     cat(sprintf("   'date'      : \"%04d-%02d-%02d/to/%04d-%02d-%02d\",\n",
                               opt$year,opt$month,1,
                               opt$year,opt$month,
              days_in_month(ymd(sprintf("%04d-%02d-01",opt$year,opt$month)))))
     cat('   \'format\'    : "netcdf",\n')
     cat(sprintf("   'target'    : \"%s\",\n",target.file))
     cat('})')
     sink()
     system(sprintf("python %s",fname))
     unlink(fname)
 }
}

s<-lapply(vars,get.surface.forecast)
