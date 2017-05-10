#!/usr/bin/env Rscript

# Pull down ERA Interim data for 1981-2010 from ECMWF (to $SCRATCH)
# Needs ECMWF account and python script access setup
# https://software.ecmwf.int/wiki/display/WEBAPI/Python+ERA-interim+examples

library(lubridate)

local.dir<-sprintf("%s/ERA_Interim",Sys.getenv('SCRATCH'))

# Surface data - analysis
#vars.a<-list(prmsl   = '151')
vars.a<-list(air.2m   = '167.128')

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

opt<-list(year=1981,month=1)
for(year in seq(1980,2011)) {
  opt$year<-year
  for(month in seq(1,12)) {
    opt$month<-month
    s<-lapply(names(vars.a),get.surface.analysis)
  }
}
