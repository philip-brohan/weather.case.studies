#!/usr/bin/env Rscript 

# Combine an ERA5 climatology for 2010-2016, an ERA Interim climatology for 2010-2016,
#  and an ERA Interim climatology for 1981-2010, to estimate an ERA5 climatology for
#  1981-2010.

# This script does this combination for a single hour.

library(GSDF.ERAI)
library(GSDF.ERA5)
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
# a) is an ERA5 climatology for 2010-2016
# b) is an ERA Interim climatology for 2010-2016,
# c) is an ERA Interim climatology for 1981-2010
#
# Get each of these fields for the given month, day, hour;
#  convert them to the grid of a)
# calculate a-b+c
# save this as the V3 climatology estimate for this time-point.

# c) is a stock product - easy to get

c<-ERAI.get.slice.at.hour(opt$variable,1981,opt$month,opt$day,opt$hour,
                          type='normal')

# a) and b) can be obtained similarly, as long as we override the
# ERA?.hourly.get.file.name functions to get the 2010-2016 climatologies
#  instead of the standard ones.

Override.hourly.get.file.name<-function(variable,year,month,day,hour,
                                        fc.init=NULL,
                                        type='mean') {
   return(ERAI.climatology.get.file.name(variable=variable,
                                         month=month,
                                         first.year=2010,
                                         last.year=2016))
}
assignInNamespace("ERAI.hourly.get.file.name",
                  Override.hourly.get.file.name,
                  ns="GSDF.ERAI")

a<-ERAI.get.slice.at.hour(opt$variable,1981,opt$month,opt$day,opt$hour,
                          type='normal')

Override.hourly.get.file.name<-function(variable,year,month,day,hour,
                                        fc.init=NULL,
                                        stream='oper',
                                        type='mean') {
   return(ERA5.climatology.get.file.name(variable=variable,
                                         month=month,
                                         first.year=2010,
                                         last.year=2016))
}
assignInNamespace("ERA5.hourly.get.file.name",
                  Override.hourly.get.file.name,
                  ns="GSDF.ERA5")

b<-ERA5.get.slice.at.hour(opt$variable,1981,opt$month,opt$day,opt$hour,
                          type='normal')

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
 
# Write to a file 
fn<-ERA5.climatology.get.file.name(opt$variable,opt$month)
if(!file.exists(dirname(fn))) dir.create(dirname(fn),recursive=TRUE)
fn<-sprintf("%s/%s.%02d.%02d.nc",dirname(fn),opt$variable,opt$day,opt$hour)
GSDF.ncdf.write(a,fn,name=ERA5.translate.for.variable.names(opt$variable))
