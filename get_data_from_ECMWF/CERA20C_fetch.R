#!/usr/bin/env Rscript

# Pull down CERA_20C data for a month from ECMWF (to $SCRATCH)
# Needs ECMWF account and python script access setup
# https://software.ecmwf.int/wiki/display/WEBAPI/Python+ERA-interim+examples

library(getopt)
library(lubridate)

opt = getopt(matrix(c(
  'year',         'y', 2, 'integer',
  'month',        'm', 2, 'integer'
), byrow=TRUE, ncol=4))

if(is.null(opt$year))     stop('Year must be specified')
if(is.null(opt$month))    stop('Month must be specified')

local.dir<-sprintf("%s/CERA_20C",Sys.getenv('SCRATCH'))

# Surface data - analysis
vars.a<-list(air.2m   = '167.128',
	     prmsl    = '151.128',
	     icec     = '31.128',
	     sst      = '34')

get.surface.analysis<-function(var) {
 fname<-tempfile()
 target.dir<-sprintf("%s/hourly/%04d/%02d",local.dir,opt$year,opt$month)
 if(!file.exists(target.dir)) dir.create(target.dir,recursive=TRUE)
 target.file<-(sprintf("%s/%s.nc",target.dir,var))
 if(file.exists(target.file) && file.info(target.file)$size>0) return() # already done
 sink(fname)
 cat('#!/usr/bin/env python\n')
 cat('from ecmwfapi import ECMWFDataServer\n')
 cat('server = ECMWFDataServer()\n')
 cat('server.retrieve({\n')
 cat('   \'dataset\'   : "cera20c",\n')
 cat('   \'stream\'    : "enda",\n')
 cat('   \'type\'      : "an",\n')
 cat('   \'class\'     : "ep",\n')
 cat('   \'expver\'    : "1",\n')
 cat('   \'levtype\'   : "sfc",\n')
 cat('   \'grid\'      : "2/2",\n')
 cat('   \'time\'      : "00/03/06/09/12/15/18/21",\n')
 cat('   \'number\'    : "0/1/2/3/4/5/6/7/8/9",\n')
 cat(sprintf("   \'param\'     : \"%s\",\n",vars.a[[var]]))
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
s<-lapply(names(vars.a),get.surface.analysis)

vars.f<-list(prate    = '228.128')
get.surface.forecast<-function(var) {
 fname<-tempfile()
 target.dir<-sprintf("%s/hourly/%04d/%02d",local.dir,opt$year,opt$month)
 if(!file.exists(target.dir)) dir.create(target.dir,recursive=TRUE)
 # Forecast starts at 18 - get the first 24 hours
 target.file<-(sprintf("%s/%s.nc",target.dir,var))
 if(!file.exists(target.file) || file.info(target.file)$size==0) { # already done
     sink(fname)
     cat('#!/usr/bin/env python\n')
     cat('from ecmwfapi import ECMWFDataServer\n')
     cat('server = ECMWFDataServer()\n')
     cat('server.retrieve({\n')
     cat('   \'dataset\'   : "cera20c",\n')
     cat('   \'stream\'    : "enda",\n')
     cat('   \'type\'      : "fc",\n')
     cat('   \'step\'      : "3/6/9/12/15/18/21/24",\n')
     cat('   \'expver\'    : "1",\n')
     cat('   \'class\'     : "ep",\n')
     cat('   \'levtype\'   : "sfc",\n')
     cat('   \'grid\'      : "1.25/1.25",\n')
     cat('   \'time\'      : "18",\n')
     cat('   \'number\'    : "0/1/2/3/4/5/6/7/8/9",\n')
     cat(sprintf("   \'param\'     : \"%s\",\n",vars.f[[var]]))
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
 # Also want the 27-hour forecast (for interpolation)
 #  has to be in a diferent file as it has the same validity time as the 3-hour forcast for the next day
 target.file<-(sprintf("%s/%s.p1d.nc",target.dir,var))
 if(file.exists(target.file) && file.info(target.file)$size>0) return() # already done
 sink(fname)
 cat('#!/usr/bin/env python\n')
 cat('from ecmwfapi import ECMWFDataServer\n')
 cat('server = ECMWFDataServer()\n')
 cat('server.retrieve({\n')
 cat('   \'dataset\'   : "cera20c",\n')
 cat('   \'stream\'    : "enda",\n')
 cat('   \'type\'      : "fc",\n')
 cat('   \'step\'      : "27",\n')
 cat('   \'expver\'    : "1",\n')
 cat('   \'class\'     : "ep",\n')
 cat('   \'levtype\'   : "sfc",\n')
 cat('   \'grid\'      : "1.25/1.25",\n')
 cat('   \'time\'      : "18",\n')
 cat('   \'number\'    : "0/1/2/3/4/5/6/7/8/9",\n')
 cat(sprintf("   \'param\'     : \"%s\",\n",vars.f[[var]]))
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
s<-lapply(names(vars.f),get.surface.forecast)
