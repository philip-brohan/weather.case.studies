#!/usr/bin/env Rscript

# Pull down 20CR data for a year from NERSC (to $SCRATCH)
# Needs password-free ssh to pbrohan account at NERSC

library(getopt)
library(parallel)

opt = getopt(matrix(c(
  'year',         'y', 2, 'integer',
  'version',      'v', 2, 'character',
  'extended',     'x', 0, "logical",
  'first.guess',  'f', 0, "logical",
  'ensemble',     'e', 0, "logical",
  'observations', 'o', 0, "logical",
  'normals',      'n', 0, "logical",
  'sds',          's', 0, "logical"
), byrow=TRUE, ncol=4))

if(is.null(opt$year))  stop('Year must be specified')
if(is.null(opt$version))      opt$version='3.5.1'
if(opt$version=='2c') opt$version='3.5.1'
if(opt$version=='2') opt$version='3.2.1'
if(is.null(opt$extended))     opt$extended=FALSE
if(is.null(opt$first.guess))  opt$first.guess=FALSE
if(is.null(opt$ensemble))     opt$ensemble=FALSE
if(is.null(opt$observations)) opt$observations=FALSE
if(is.null(opt$normals))      opt$normals=FALSE
if(is.null(opt$sds))          opt$sds=FALSE

local.dir<-sprintf("%s/20CR/version_%s",Sys.getenv('SCRATCH'),opt$version)

# Ensemble mean and spread
get.base<-function(var) {
 remote.dir<-sprintf("pbrohan@dtn02.nersc.gov:/project/projectdirs/m958/netCDF.data/20CR_v%s/",opt$version)
 local.file<-sprintf("%s/hourly/%s/%s.%04d.nc",local.dir,var,var,opt$year)
 if(!file.exists(dirname(local.file))) dir.create(dirname(local.file),recursive=TRUE)
 if(!file.exists(local.file)) {
    cmd<-sprintf("scp %s/hourly/%s/%s.%04d.nc %s",remote.dir,
                 var,var,opt$year,local.file)
    system(cmd)
    if(!file.exists(local.file)) return(sprintf("Failed: %s",cmd))
  }
 local.file<-sprintf("%s/hourly/%s/%s.%04d.spread.nc",local.dir,var,var,opt$year)
 if(!file.exists(local.file)) {
    cmd<-sprintf("scp %s/hourly/%s/%s.%04d.spread.nc %s",remote.dir,
                 var,var,opt$year,local.file)
    system(cmd)
    if(!file.exists(local.file)) return(sprintf("Failed: %s",cmd))
  }
  return(TRUE)
}
 
# Base set - always get
vars<-c('air.2m','icec','prate','prmsl','uwnd.10m','vwnd.10m')
get.result<-mclapply(vars,get.base,mc.cores=6)
w<-which(get.result!=TRUE)
if(length(w)>0) {
  for(i in w) print(get.result[[i]])
  stop("Base download failed")
}

# Extended set
if(opt$extended==TRUE) {
    vars<-c('air.sfc')
    get.result<-mclapply(vars,get.base)
    w<-which(get.result!=TRUE)
    if(length(w)>0) {
      for(i in w) print(get.result[[i]])
      stop("Extended download failed")
    }
}

# Full ensemble
get.members<-function(var) {
 remote.dir<-sprintf("pbrohan@dtn02.nersc.gov:/project/projectdirs/m958/netCDF.data/20CR_v%s/",opt$version)
 if(opt$version=='3.5.1') {
    remote.dir<-"pbrohan@dtn02.nersc.gov:/project/projectdirs/20C_Reanalysis/www/20C_Reanalysis_version2c_ensemble/"
 }
 if(opt$version=='3.2.1') {
    remote.dir<-"pbrohan@dtn02.nersc.gov:/project/projectdirs/20C_Reanalysis/www/20C_Reanalysis_ensemble/"
 }  
 local.file<-sprintf("%s/ensembles/hourly/%s/%s.%04d.nc",local.dir,var,var,opt$year)
 if(!file.exists(dirname(local.file))) dir.create(dirname(local.file),recursive=TRUE)
 if(!file.exists(local.file)) {
    cmd<-''
    if(opt$version=='3.5.1' || opt$version=='3.2.1') {
        if(var=='prmsl') {
           cmd<-sprintf("scp %s/analysis/%s/%s_%04d.nc %s",remote.dir,
                        var,var,opt$year,local.file)
         }
        if(var=='air.2m') {
           cmd<-sprintf("scp %s/analysis/%s/%s_%04d.nc %s",remote.dir,
                        't9950','t9950',opt$year,local.file)
         }
        if(var=='uwnd.10m') {
           cmd<-sprintf("scp %s/first_guess/%s/%s_%04d.nc %s",remote.dir,
                        'u10m','u10m',opt$year,local.file)
         }
        if(var=='vwnd.10m') {
           cmd<-sprintf("scp %s/first_guess/%s/%s_%04d.nc %s",remote.dir,
                        'v10m','v10m',opt$year,local.file)
         }     
        if(var=='prate') {
           cmd<-sprintf("scp %s/first_guess/%s/%s_%04d.nc %s",remote.dir,
                        var,var,opt$year,local.file)
     }
    } else {
      cmd<-sprintf("scp %s/ensembles/hourly/%s/%s.%04d.nc %s",remote.dir,
                        var,var,opt$year,local.file)
    }
    system(cmd)
    if(!file.exists(local.file)) return(sprintf("Failed: %s",cmd))
  }
  return(TRUE)
}
if(opt$ensemble==TRUE) {
    vars<-c('air.2m','prate','prmsl','uwnd.10m','vwnd.10m')
    get.result<-mclapply(vars,get.members,mc.cores=5)
    w<-which(get.result!=TRUE)
    if(length(w)>0) {
      for(i in w) print(get.result[[i]])
      stop("Ensemble download failed")
    }
}

# Normals
get.normals<-function(var) {
 remote.dir<-sprintf("pbrohan@dtn02.nersc.gov:/project/projectdirs/m958/netCDF.data/20CR_v%s/",opt$version)
 local.file<-sprintf("%s/hourly/normals/%s.nc",local.dir,var)
 if(!file.exists(dirname(local.file))) dir.create(dirname(local.file),recursive=TRUE)
 if(!file.exists(local.file)) {
    cmd<-sprintf("scp %s/hourly/normals/%s.nc %s",remote.dir,
                 var,local.file)
    system(cmd)
    if(!file.exists(local.file)) return(sprintf("Failed: %s",cmd))
  }
  return(TRUE)
}
if(opt$normals==TRUE) {
    vars<-c('air.2m','icec','prate','prmsl','uwnd.10m','vwnd.10m','air.sfc','runoff','snowc','soilm')
    get.result<-mclapply(vars,get.normals,mc.cores=5)
    w<-which(get.result!=TRUE)
    if(length(w)>0) {
      for(i in w) print(get.result[[i]])
      stop("Normals download failed")
    }
}

# Standard deviations
get.sds<-function(var) {
 remote.dir<-sprintf("pbrohan@dtn02.nersc.gov:/project/projectdirs/m958/netCDF.data/20CR_v%s/",opt$version)
 local.file<-sprintf("%s/hourly/standard.deviations/%s.nc",local.dir,var)
 if(!file.exists(dirname(local.file))) dir.create(dirname(local.file),recursive=TRUE)
 if(!file.exists(local.file)) {
    cmd<-sprintf("scp %s/hourly/standard.deviations/%s.nc %s",remote.dir,
                 var,local.file)
    system(cmd)
    if(!file.exists(local.file)) return(sprintf("Failed: %s",cmd))
  }
  return(TRUE)
}
if(opt$sds==TRUE) {
    vars<-c('air.2m','icec','prate','prmsl','uwnd.10m','vwnd.10m','air.sfc','runoff','snowc','soilm')
    get.result<-mclapply(vars,get.sds,mc.cores=5)
    w<-which(get.result!=TRUE)
    if(length(w)>0) {
      for(i in w) print(get.result[[i]])
      stop("sds download failed")
    }
}

# First Guess
get.first.guess<-function(var) {
 remote.dir<-sprintf("pbrohan@dtn02.nersc.gov:/project/projectdirs/m958/netCDF.data/20CR_v%s/",opt$version)
 local.file<-sprintf("%s/first.guess.hourly/%s/%s.%04d.nc",local.dir,var,var,opt$year)
 if(!file.exists(dirname(local.file))) dir.create(dirname(local.file),recursive=TRUE)
 if(!file.exists(local.file)) {
    cmd<-sprintf("scp %s/first.guess.hourly/%s/%s.%04d.nc %s",remote.dir,
                 var,var,opt$year,local.file)
    system(cmd)
    if(!file.exists(local.file)) return(sprintf("Failed: %s",cmd))
  }
 local.file<-sprintf("%s/first.guess.hourly/%s/%s.%04d.spread.nc",local.dir,var,var,opt$year)
 if(!file.exists(local.file)) {
    cmd<-sprintf("scp %s/first.guess.hourly/%s/%s.%04d.spread.nc %s",remote.dir,
                 var,var,opt$year,local.file)
    system(cmd)
    if(!file.exists(local.file)) return(sprintf("Failed: %s",cmd))
  }
  return(TRUE)
}
if(opt$first.guess==TRUE) {
    vars<-c('air.2m','prmsl','uwnd.10m','vwnd.10m')
    get.result<-mclapply(vars,get.first.guess,mc.cores=4)
    w<-which(get.result!=TRUE)
    if(length(w)>0) {
      for(i in w) print(get.result[[i]])
      stop("First guess download failed")
    }
}


# observations 
get.observations<-function(month) {
   remote.dir<-sprintf("pbrohan@dtn02.nersc.gov:/project/projectdirs/m958/netCDF.data/20CR_v%s/",opt$version)
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
if(opt$observations==TRUE) {
   get.result<-get.observations()
       w<-which(get.result!=TRUE)
    if(length(w)>0) {
      for(i in w) print(get.result[[i]])
      stop("Observations download failed")
    }
}  

# set permissions for all to read
cmd<-sprintf("chmod -R a+rX %s",local.dir)
system(cmd)
