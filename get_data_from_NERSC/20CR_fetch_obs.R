#!/usr/bin/Rscript

# Pull down 20CR obs from NERSC (to $SCRATCH)
# Needs password-free ssh to pbrohan account at NERSC

library(getopt)
library(parallel)

version<-'3.5.1'

local.dir<-sprintf("%s/20CR/version_%s",Sys.getenv('SCRATCH'),version)

# observations 
get.observations<-function() {
   remote.dir<-sprintf("pbrohan@dtn02.nersc.gov:/project/projectdirs/m958/netCDF.data/20CR_v%s/",version)
   local.obs.dir<-sprintf("%s/observations",local.dir)
   if(!file.exists(dirname(local.obs.dir))) dir.create(dirname(local.obs.dir),recursive=TRUE)
   cmd<-sprintf("rsync -avzL %s/observations/%04d %s/",remote.dir,opt$year,local.obs.dir)
   system(cmd)
  # check one arbitrary file
   if(!file.exists(sprintf("%s/%04d/prepbufrobs_assim_%04d060500.txt",local.obs.dir,opt$year,opt$year))) {
     return(sprintf("Failed: %s",cmd))
   }
   return(TRUE)
}
opt<-list()
for(year in seq(1853,2014)) {
  opt$year<-year
   get.result<-get.observations()
       w<-which(get.result!=TRUE)
    if(length(w)>0) {
      for(i in w) print(get.result[[i]])
      stop("Observations download failed")
    }
  # set permissions for all to read
  cmd<-sprintf("chmod -R a+xR %s/observations/%04d",local.dir,year)
  system(cmd)
} 
  
