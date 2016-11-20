#!/usr/bin/env Rscript

# Pull down ERA Interim data for a month from ECMWF (to $SCRATCH)
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

local.dir<-sprintf("%s/ERA_Interim",Sys.getenv('SCRATCH'))

# Surface data - analysis
vars.a<-list(air.2m   = '167.128',
           uwnd.10m = '165.128',
	   vwnd.10m = '166.128',
	   prmsl    = '151',
	   icec     = '31',
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
 cat('   \'dataset\'   : "interim",\n')
 cat("   'stream'    : \"oper\",\n")
 cat('   \'type\'      : "an",\n')
 cat('   \'step\'      : "0",\n')
 cat('   \'class\'     : "ei",\n')
 cat('   \'levtype\'   : "sfc",\n')
 cat(sprintf("   \'param\'     : \"%s\",\n",vars.a[[var]]))
 cat('   \'grid\'      : "0.75/0.75",\n')
 cat('   \'time\'      : "00/06/12/18",\n')
 cat(sprintf("   'date'      : \"%04d-%02d-%02d/to/%04d-%02d-%02d\",\n",
                               opt$year,opt$month,1,
			       opt$year,opt$month,days_in_month(ymd(sprintf("%04d-%02d-01",opt$year,opt$month)))))
 cat('   \'format\'    : "netcdf",\n')
 cat(sprintf("   'target'    : \"%s\",\n",target.file))
 cat('})')
 sink()
 system(sprintf("python %s",fname))
 unlink(fname)
}
s<-lapply(names(vars.a),get.surface.analysis)

vars.f<-list(prate    = '228')
get.surface.forecast<-function(var) {
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
 cat('   \'dataset\'   : "interim",\n')
 cat("   'stream'    : \"oper\",\n")
 cat('   \'type\'      : "fc",\n')
 cat('   \'step\'      : "3/6/9/12",\n')
 cat('   \'class\'     : "ei",\n')
 cat('   \'levtype\'   : "sfc",\n')
 cat(sprintf("   \'param\'     : \"%s\",\n",vars.f[[var]]))
 cat('   \'grid\'      : "0.75/0.75",\n')
 cat('   \'time\'      : "00/12",\n')
 cat(sprintf("   'date'      : \"%04d-%02d-%02d/to/%04d-%02d-%02d\",\n",
                               opt$year,opt$month,1,
			       opt$year,opt$month,days_in_month(ymd(sprintf("%04d-%02d-01",opt$year,opt$month)))))
 cat('   \'format\'    : "netcdf",\n')
 cat(sprintf("   'target'    : \"%s\",\n",target.file))
 cat('})')
 sink()
 system(sprintf("python %s",fname))
 unlink(fname)
}
s<-lapply(names(vars.f),get.surface.forecast)
