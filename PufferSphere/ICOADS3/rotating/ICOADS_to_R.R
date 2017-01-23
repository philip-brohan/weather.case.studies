#!/usr/bin/Rscript --no-save

# Convert ICOADS files from IMMA to R format
# To cache and speed up access

library(getopt)
library(IMMA)

in.dir<-sprintf("%s/icoads_3.0_beta_4",Sys.getenv('LOCALDATA'))
out.dir<-sprintf("%s/icoads_3.0_beta_4_R",Sys.getenv('SCRATCH'))
if(!file.exists(out.dir)) dir.create(out.dir,recursive=TRUE)

opt = getopt(c(
  'year',   'd', 2, "integer",
  'month',  'm', 2, "integer"
));
if ( is.null(opt$year) )   { stop("Year not specified") }
if ( is.null(opt$month) )  { stop("Month not specified") }

in.file.name<-sprintf("%s/IMMA1.%04d.%02d.gz",in.dir,opt$year,opt$month)
obs<-ReadObs(in.file.name)
out.file.name<-sprintf("%s/%04d.%02d.Rdata",out.dir,opt$year,opt$month)
saveRDS(obs,file=out.file.name)
