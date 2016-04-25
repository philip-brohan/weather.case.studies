#!/usr/bin/Rscript

# 1976 drought

library(GSDF.TWCR)
library(GSDF.WeatherMap)
library(parallel)

Year<-1976
Month<-6
Day<-1
Hour<-0
n.total<-(30+31+31+30)*24*6 # Total number of timesteps to be rendered
version<-'3.5.1'
fog.threshold<-exp(1)

Sys.setenv(SCRATCH='/scratch/hadpb')
GSDF.cache.dir<-sprintf("%s/GSDF.cache",Sys.getenv('SCRATCH'))
if(!file.exists(GSDF.cache.dir)) dir.create(GSDF.cache.dir,recursive=TRUE)
Imagedir<-sprintf("%s/images/1976-summer",Sys.getenv('SCRATCH'),version)
if(!file.exists(Imagedir)) dir.create(Imagedir,recursive=TRUE)

c.date<-chron(dates=sprintf("%04d/%02d/%02d",Year,Month,Day),
          times=sprintf("%02d:00:00",Hour),
          format=c(dates='y/m/d',times='h:m:s'))

Options<-WeatherMap.set.option(NULL)
Options<-WeatherMap.set.option(Options,'show.mslp',T)
Options<-WeatherMap.set.option(Options,'show.ice',F)
Options<-WeatherMap.set.option(Options,'show.obs',F)
Options<-WeatherMap.set.option(Options,'show.fog',F)
Options<-WeatherMap.set.option(Options,'show.precipitation',T)
Options<-WeatherMap.set.option(Options,'temperature.range',6)
Options<-WeatherMap.set.option(Options,'precip.points',50000)
Options<-WeatherMap.set.option(Options,'precip.range',0.015)
Options<-WeatherMap.set.option(Options,'precip.min.transparency',0.85)

range<-15
aspect<-4/3
Options<-WeatherMap.set.option(Options,'lat.min',range*-1)
Options<-WeatherMap.set.option(Options,'lat.max',range)
Options<-WeatherMap.set.option(Options,'lon.min',range*aspect*-1)
Options<-WeatherMap.set.option(Options,'lon.max',range*aspect)
Options<-WeatherMap.set.option(Options,'pole.lon',173)
Options<-WeatherMap.set.option(Options,'pole.lat',36)

land<-WeatherMap.get.land(Options)
Options$wind.vector.lwd<-3
Options$wind.vector.scale<-0.5
Options$wind.vector.density<-1.5
Options$wind.vector.move.scale<-10

Options$mslp.lwd<-3
Options$mslp.base=0                    # Base value for anomalies
Options$mslp.range=50000                    # Anomaly for max contour
Options$mslp.step=250                       # Smaller -more contours
Options$mslp.tpscale=3500                    # Smaller -contours less transparent

get.member.at.hour<-function(variable,year,month,day,hour,member) {

       t<-TWCR.get.members.slice.at.hour(variable,year,month,day,
                                  hour,version=version)
       t<-GSDF.select.from.1d(t,'ensemble',member)
       gc()
       return(t)
  }


make.streamlines<-function(year,month,day,hour,streamlines=NULL) {

    sf.name<-sprintf("%s/streamlines.%04d-%02d-%02d:%02d:%02d.rd",
                           Imagedir,year,month,day,as.integer(hour),
                                               as.integer(hour%%1*60))
    if(file.exists(sf.name) && file.info(sf.name)$size>500000) {
       load(sf.name)
       return(s)
    }
    print(sprintf("%04d-%02d-%02d:%02d:%02d - %s",year,month,day,as.integer(hour),
                   as.integer(hour%%1*60),
                   Sys.time()))

    uwnd<-get.member.at.hour('uwnd.10m',year,month,day,hour,member=1)
    vwnd<-get.member.at.hour('vwnd.10m',year,month,day,hour,member=1)
    t.actual<-get.member.at.hour('air.2m',year,month,day,hour,member=1)
    t.normal<-t.actual
    t.normal$data[]<-rep(283,length(t.normal$data))
    s<-WeatherMap.make.streamlines(streamlines,uwnd,vwnd,t.actual,t.normal,Options)
    save(year,month,day,hour,s,file=sf.name)
    gc(verbose=FALSE)
    return(s)

}

plot.hour<-function(date) {

      sink('multistart.step.slm')
      cat('#!/bin/ksh -l\n')
      cat('#SBATCH --qos=normal\n')
      cat('#SBATCH --output=/scratch/hadpb/slurm_output/1976_summer_%j.out\n')
      cat('#SBATCH --mem=5000\n')
      cat('#SBATCH --ntasks=1\n')
      cat('#SBATCH --ntasks-per-core=2\n')
      cat('#SBATCH --time=5\n')
      cat(sprintf("./plot_frame.R --date=%s\n",date))
      sink()
      system('sbatch multistart.step.slm')
}

s<-NULL
jobs<-list()
for(n.count in seq(0,n.total)) {

    n.date<-c.date+n.count/(24*6)
    year<-as.numeric(as.character(years(n.date)))
    month<-months(n.date)
    day<-days(n.date)
    hour<-((n.count+Hour)%%(24*6))/6

    # serial component - streamlines evolve from hour to hour
    s<-make.streamlines(year,month,day,hour,streamlines=s)

    
    date.string<-sprintf("%04d-%02d-%02d:%02d:%02d",year,month,day,
                               as.integer(hour),as.integer((hour%%1)*60))
    
    image.name<-sprintf("%s.png",date.string)
    ifile.name<-sprintf("%s/%s",Imagedir,image.name)
    if(file.exists(ifile.name) && file.info(ifile.name)$size>0) next

    plot.hour(date.string)
    gc(verbose=FALSE)

}
