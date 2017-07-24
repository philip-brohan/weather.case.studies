#!/usr/bin/env Rscript 

# Combine a V3 climatology for 1916-1919, a V2c climatology for 1916-1919,
#  and a V2c climatology for 1981-2010, to estimate a V3 climatology for
#  1981-2010.

# This script does this combination for a single hour.

library(GSDF.TWCR)
library(getopt)
opt = getopt(matrix(c(
  'variable', 'v', 2, "character",
  'month',    'm', 2, "integer",
  'day',      'd', 2, "integer",
  'hour',     'h', 2, "numeric"
), byrow=TRUE, ncol=4))
if ( is.null(opt$variable) )  { stop("Variable not specified") }
if ( is.null(opt$month) )     { stop("Month not specified") }
if ( is.null(opt$day) )       { stop("Day not specified") }
if ( is.null(opt$hour) )      { stop("Hour not specified") }

# If
# a) is a V3 climatology for 1916-1919
# b) is a V2c climatology for 1916-1919,
# c) is a V2c climatology for 1981-2010
#
# Get each of these fields for the given month, day, hour;
#  convert them to the grid of a)
# calculate a-b+c
# save this as the V3 climatology estimate for this time-point.

# c) is a stock product - easy to get

c<-TWCR.get.slice.at.hour(opt$variable,1981,opt$month,opt$day,opt$hour,
                          version='3.4.1',type='normal')

# a) and b) can be obtained similarly, as long as we override the
# TWCR.hourly.get.file.name function to get the 1916-1919 climatologies
#  instead of the standard ones.

Override.hourly.get.file.name<-function(variable,year,month,day,hour,
                                        height=NULL,
                                        opendap=NULL,version=2,type='mean') {
   return(TWCR.climatology.get.file.name(variable=variable,
                                         month=month,
                                         version=version,
                                         first.year=1916,
                                         last.year=1919))
}
assignInNamespace("TWCR.hourly.get.file.name",
                  Override.hourly.get.file.name,
                  ns="GSDF.TWCR")

a<-TWCR.get.slice.at.hour(opt$variable,1981,opt$month,opt$day,opt$hour,
                          version='4.1.8',type='normal')
b<-TWCR.get.slice.at.hour(opt$variable,1981,opt$month,opt$day,opt$hour,
                          version='3.5.1',type='normal')

# Regrid
b<-GSDF.regrid.2d(b,a)
c<-GSDF.regrid.2d(c,a)

# Combine
a$data[]<-as.vector(a$data)-as.vector(b$data)+as.vector(c$data)

# Set the meadata to new standard
a$meta$calendar='gregorian'
t.i<-GSDF.find.dimension(a,'time')
a$dimensions[[t.i]]$values<-sprintf("1981-%02d-%02d:%02d:%02d",
                                     as.integer(opt$month),
                                     as.integer(opt$day),
                                     as.integer(opt$hour),0)
 
# Write to a file - I'm calling it the climatology for version 4.0.0 just
#  to provide a clear ID.
fn<-TWCR.climatology.get.file.name(opt$variable,opt$month,'4.0.0')
if(!file.exists(dirname(fn))) dir.create(dirname(fn),recursive=TRUE)
fn<-sprintf("%s/%s.%02d.%02d.nc",dirname(fn),opt$variable,opt$day,opt$hour)
GSDF.ncdf.write(a,fn,name=opt$variable)
