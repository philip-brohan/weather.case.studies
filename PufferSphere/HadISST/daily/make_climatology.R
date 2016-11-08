# Make a 1981-2010 climatology from HadISST2.2.0.0.
#  Store the results as Rdata files

library(GSDF)
library(parallel)
library(lubridate)

c.date<-ymd("1981/01/01")

HadISST.get.sst.at.month<-function(year,month,day,member) {
  file<-sprintf("%s/HadISST.2.2.0.0/Realisation_%d/HadISST2_preliminary_frompentad_qxqxd_SST_and_ice_%d_%d_%04d.nc",
                Sys.getenv('SCRATCH'),member,day,month,year)
  field<-GSDF.ncdf.load(file,'sst',
                        time.range=ymd(sprintf("%04d-%02d-01",1800,1),
                                       sprintf("%04d-%02d-31",2050,12)),
                        lat.range=c(-90,90),lon.range=c(-180,360))
  field$meta$pole.lon<-180
  w<-which(field$data< -100)
  is.na(field$data[w])<-TRUE
  field$dimensions[[2]]$values<-field$dimensions[[2]]$values*-1
  return(field)
}

make.clim<-function(n.count) {
  
   n.date<-c.date+days(n.count)
   Month<-month(n.date)
   Day<-day(n.date)


     print(sprintf("%02d-%02d",Month,Day))
     
      t<-HadISST.get.sst.at.month(1981,3,12,1)
      data.length<-1
   
      for(Year in seq(1982,2010)) {
         p<-HadISST.get.sst.at.month(Year,Month,Day,1)    
         t$data<-t$data+p$data
         data.length<-data.length+1

      }
      
      f.name<-sprintf("%s/HadISST.2.2.0.0/climatology_%02d%02d.Rdata",
                            Sys.getenv('SCRATCH'),Month,Day)
      t$data<-t$data/data.length
      saveRDS(t,f.name)

    gc(verbose=F)
 }
     
f<-mclapply(seq(0,364),make.clim,mc.cores=8,mc.preschedule=FALSE)
