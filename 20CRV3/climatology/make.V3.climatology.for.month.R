#!/usr/bin/env Rscript 

#  Combine a V3 climatology for 1916-1919, a V2c climatology for 1916-1919,
#  and a V2c climatology for 1981-2010, to estimate a V3 climatology for
#  1981-2010.

# This script runs make.V3.climatology.at.point.R repeatedly to make all the
# point climatologies, uses ncrcat to combine them all into a single
# monthly file, and cleans up all the intermediate files.

library(lubridate)
library(getopt)
library(GSDF.TWCR)

opt = getopt(matrix(c(
  'variable', 'v', 2, "character",
  'month',    'm', 2, "integer"
), byrow=TRUE, ncol=4))
if ( is.null(opt$variable) )  { stop("Variable not specified") }
if ( is.null(opt$month) )     { stop("Month not specified") }

c.time<-lubridate::ymd_hms(sprintf("1981-%02d-01:00:00:00",opt$month))
e.time<-c.time+months(1)
while(c.time<e.time) {
  cmd<-sprintf("./make.V3.climatology.at.point.R --variable=%s --month=%d --day=%d --hour=%d",
               opt$variable,
               as.integer(lubridate::month(c.time)),
               as.integer(lubridate::day(c.time)),
               as.integer(lubridate::hour(c.time)))
  system(cmd)
  c.time<-c.time+lubridate::hours(3)
}

fn<-TWCR.climatology.get.file.name(opt$variable,opt$month,
                                   version='4.0.0')
cmd<-sprintf("ncrcat -O %s/%s.*.*.nc %s",dirname(fn),opt$variable,fn)
system(cmd)
unlink(sprintf("%s/%s.*.*.nc",dirname(fn),opt$variable))
