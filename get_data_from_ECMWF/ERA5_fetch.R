#!/usr/bin/env Rscript

# Pull down ERA5 data for a day from ECMWF (to $SCRATCH)
# Needs ECMWF account and python script access setup
# https://software.ecmwf.int/wiki/display/CKB/How+to+download+ERA5+test+data+via+the+ECMWF+Web+API

library(getopt)
library(parallel)

opt = getopt(matrix(c(
  'year',           'y', 2, 'integer',
  'month',          'm', 2, 'integer',
  'day',            'd', 2, 'integer',
  'pressure.level', 'h', 2, 'logical',
  'stream',         's', 2, "character"
), byrow=TRUE, ncol=4))

if(is.null(opt$year))           stop('Year must be specified')
if(is.null(opt$month))          stop('Month must be specified')
if(is.null(opt$day))            stop('Day must be specified')
if(is.null(opt$stream))         opt$stream='oper'
if(is.null(opt$pressure.level)) opt$pressure.level=FALSE

local.dir<-sprintf("%s/ERA5/",Sys.getenv('SCRATCH'))

# Surface data - analysis
vars<-c('2t','10u','10v','msl','ci','sst')

get.surface.analysis<-function(var) {
 fname<-tempfile()
 target.dir<-sprintf("%s/%s/hourly/%04d/%02d/%02d",local.dir,opt$stream,opt$year,opt$month,opt$day)
 if(!file.exists(target.dir)) dir.create(target.dir,recursive=TRUE)
 target.file<-(sprintf("%s/%s.nc",target.dir,var))
 if(file.exists(target.file) && file.info(target.file)$size>0) return() # already done
 sink(fname)
 cat('#!/usr/bin/env python\n')
 cat('from ecmwfapi import ECMWFDataServer\n')
 cat('server = ECMWFDataServer()\n')
 cat('server.retrieve({\n')
 cat('   \'dataset\'   : "era5_test",\n')
 cat(sprintf("   'stream'    : \"%s\",\n",opt$stream))
 cat('   \'type\'      : "an",\n')
 cat('   \'levtype\'   : "sfc",\n')
 cat(sprintf("   \'param\'     : \"%s\",\n",var))
 cat('   \'grid\'      : "0.25/0.25",\n')
 cat('   \'time\'      : "00/01/02/03/04/05/06/07/08/09/10/11/12/13/14/15/16/17/18/19/20/21/22/23",\n')
 cat(sprintf("   'date'      : \"%04d-%02d-%02d/to/%04d-%02d-%02d\",\n",
                               opt$year,opt$month,opt$day,
			       opt$year,opt$month,opt$day))
 cat('   \'format\'    : "netcdf",\n')
 cat(sprintf("   'target'    : \"%s\",\n",target.file))
 cat('})')
 sink()
 system(sprintf("python %s",fname))
 unlink(fname)
}
s<-lapply(vars,get.surface.analysis)

# Surface data - forecast
vars<-c('tp')

get.surface.forecast<-function(var) {
 fname<-tempfile()
 target.dir<-sprintf("%s/%s/hourly/%04d/%02d/%02d",local.dir,opt$stream,opt$year,opt$month,opt$day)
 if(!file.exists(target.dir)) dir.create(target.dir,recursive=TRUE)
 for(start.hour in c(6,18)) {
     target.file<-(sprintf("%s/%s.%02d.nc",target.dir,var,start.hour))
     if(file.exists(target.file) && file.info(target.file)$size>38000000) return() # already done
     sink(fname)
     cat('#!/usr/bin/env python\n')
     cat('from ecmwfapi import ECMWFDataServer\n')
     cat('server = ECMWFDataServer()\n')
     cat('server.retrieve({\n')
     cat('   \'dataset\'   : "era5_test",\n')
     cat(sprintf("   'stream'    : \"%s\",\n",opt$stream))
     cat('   \'type\'      : "fc",\n')
     cat('   \'step\'      : "0/to/18/by/1",\n')
     cat('   \'levtype\'   : "sfc",\n')
     cat(sprintf("   \'param\'     : \"%s\",\n",var))
     cat('   \'grid\'      : "0.25/0.25",\n')
     cat(sprintf("   \'time\'      : \"%02d\",\n",start.hour))
     cat(sprintf("   'date'      : \"%04d-%02d-%02d/to/%04d-%02d-%02d\",\n",
                                   opt$year,opt$month,opt$day,
                                   opt$year,opt$month,opt$day))
     cat('   \'format\'    : "netcdf",\n')
     cat(sprintf("   'target'    : \"%s\",\n",target.file))
     cat('})')
     sink()
     system(sprintf("python %s",fname))
     unlink(fname)
 }
}

s<-lapply(vars,get.surface.forecast)

# Pressure level data - analysis
vars<-c('t','u','v','z','r')

get.pressure.level.analysis<-function(var) {
 fname<-tempfile()
 target.dir<-sprintf("%s/%s/hourly/%04d/%02d/%02d",local.dir,opt$stream,opt$year,opt$month,opt$day)
 if(!file.exists(target.dir)) dir.create(target.dir,recursive=TRUE)
 target.file<-(sprintf("%s/%s.nc",target.dir,var))
 if(file.exists(target.file) && file.info(target.file)$size>0) return() # already done
 sink(fname)
 cat('#!/usr/bin/env python\n')
 cat('from ecmwfapi import ECMWFDataServer\n')
 cat('server = ECMWFDataServer()\n')
 cat('server.retrieve({\n')
 cat('   \'dataset\'   : "era5_test",\n')
 cat(sprintf("   'stream'    : \"%s\",\n",opt$stream))
 cat('   \'type\'      : "an",\n')
 cat('   \'levtype\'   : "pl",\n')
 cat(sprintf("   \'param\'     : \"%s\",\n",var))
 cat('   \'grid\'      : "0.25/0.25",\n')
 cat('   \'time\'      : "00/01/02/03/04/05/06/07/08/09/10/11/12/13/14/15/16/17/18/19/20/21/22/23",\n')
 cat(sprintf("   'date'      : \"%04d-%02d-%02d/to/%04d-%02d-%02d\",\n",
                               opt$year,opt$month,opt$day,
			       opt$year,opt$month,opt$day))
 cat('   \'format\'    : "netcdf",\n')
 cat(sprintf("   'target'    : \"%s\",\n",target.file))
 cat('})')
 sink()
 system(sprintf("python %s",fname))
 unlink(fname)
}
if(opt$pressure.level) {
   s<-lapply(vars,get.pressure.level.analysis)
}