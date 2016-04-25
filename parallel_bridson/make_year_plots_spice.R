#!/usr/bin/Rscript --no-save

# Show wind and temperature in 20CR2c
# Spherical version with garish colours
# Move the north pole to centre UK - shows up better on PufferSphere?

library(GSDF.TWCR)
library(GSDF.WeatherMap)
library(chron)

Year<-2014
Month<-1
Day<-2
Hour<-0
n.total<-500#365*24*3
version<-'3.5.1'

fog.threshold<-exp(1)

Imagedir<-sprintf("/scratch/hadpb/images/TWCR_parallel_bridson/")
if(!file.exists(Imagedir)) dir.create(Imagedir,recursive=TRUE)

c.date<-chron(dates=sprintf("%04d/%02d/%02d",Year,Month,Day),
          times=sprintf("%02d:00:00",Hour),
          format=c(dates='y/m/d',times='h:m:s'))

Options<-WeatherMap.set.option(NULL)
Options<-WeatherMap.set.option(Options,'lat.min',-90)
Options<-WeatherMap.set.option(Options,'lat.max',90)
Options<-WeatherMap.set.option(Options,'lon.min',-190)
Options<-WeatherMap.set.option(Options,'lon.max',190)
Options$vp.lon.min<- -180
Options$vp.lon.max<-  180
Options<-WeatherMap.set.option(Options,'jitter',FALSE)
Options<-WeatherMap.set.option(Options,'wrap.spherical',T)
Options<-WeatherMap.set.option(Options,'show.mslp',F)
Options<-WeatherMap.set.option(Options,'show.ice',F)
Options<-WeatherMap.set.option(Options,'show.obs',F)
Options<-WeatherMap.set.option(Options,'show.fog',F)
Options<-WeatherMap.set.option(Options,'show.precipitation',F)
Options<-WeatherMap.set.option(Options,'temperature.range',25)
Options<-WeatherMap.set.option(Options,'wind.palette',
                       diverge_hcl(7,c=c(150,0),l=c(25,30),power=1))
Options<-WeatherMap.set.option(Options,'obs.size',1.5)
Options<-WeatherMap.set.option(Options,'obs.colour',rgb(255,215,0,255,
                                                       maxColorValue=255))
Options<-WeatherMap.set.option(Options,'precip.colour',c(0.1,0.2,0.1))
Options<-WeatherMap.set.option(Options,'precip.range',0.01)
#Options<-WeatherMap.set.option(Options,'precip.points',10000)
Options<-WeatherMap.set.option(Options,'pole.lon',160)
Options<-WeatherMap.set.option(Options,'pole.lat',35)
Options<-WeatherMap.set.option(Options,'background.resolution','high')

aspect<-2

Options$ice.points<-100000
Options$wind.vector.lwd<-2.5
Options$wind.vector.move.scale<-Options$wind.vector.move.scale/3
Options$wind.vector.density<-Options$wind.vector.density*0.5

Options$cores<-10

make.streamlines<-function(year,month,day,hour,streamlines=NULL) {


    sf.name<-sprintf("%s/streamlines.%04d-%02d-%02d:%02d:%02d.rd",
                           Imagedir,year,month,day,as.integer(hour),as.integer((hour%%1)*60))
    if(file.exists(sf.name) && file.info(sf.name)$size>5000) {
       load(sf.name)
       return(s)
    }
    print(sprintf("%04d-%02d-%02d:%02d:%02d - %s",year,month,day,
                   as.integer(hour),as.integer((hour%%1)*60),Sys.time()))

    uwnd<-TWCR.get.slice.at.hour('uwnd.10m',year,month,day,hour,version=version)
    vwnd<-TWCR.get.slice.at.hour('vwnd.10m',year,month,day,hour,version=version)
    t.actual<-TWCR.get.slice.at.hour('air.2m',year,month,day,hour,version=version)
    t.normal<-t.actual
    t.normal$data[]<-rep(286,length(t.normal$data))
    s<-WeatherMap.make.streamlines(streamlines,uwnd,vwnd,t.actual,t.normal,Options)
    save(year,month,day,hour,s,file=sf.name)
    gc(verbose=FALSE)
    return(s)

}

plot.hour<-function(date) {

      sink('multistart.step.slm')
      cat('#!/bin/ksh -l\n')
      cat('#SBATCH --qos=normal\n')
      #cat('#SBATCH --output=/scratch/hadpb/slurm_output_fade_io/%j.out\n')
      cat('#SBATCH --output=/scratch/hadpb/slurm_output/%j.out\n')
      cat('#SBATCH --mem=5000\n')
      cat('#SBATCH --ntasks=1\n')
      cat('#SBATCH --ntasks-per-core=2\n')
      cat('#SBATCH --time=5\n')
      cat(sprintf("./make_year_plots_single_spice.R --date=%s\n",date))
      sink()
      system('sbatch multistart.step.slm')
}

s<-NULL
jobs<-list()
for(n.count in seq(0,n.total)) {

    n.date<-c.date+n.count/(24*3)
    year<-as.numeric(as.character(years(n.date)))
    if(year>2014) next
    month<-months(n.date)
    day<-days(n.date)
    hour<-((n.count+Hour)%%(24*3))/3

    # serial component - streamlines evolve from hour to hour
    s<-make.streamlines(year,month,day,hour,streamlines=s)

    date<-sprintf("%04d-%02d-%02d:%02d:%02d",year,month,day,
                               as.integer(hour),as.integer((hour%%1)*60))

    image.name<-sprintf("%s.png",date)
    ifile.name<-sprintf("%s/%s",Imagedir,image.name)
    if(file.exists(ifile.name) && file.info(ifile.name)$size>0) next

    plot.hour(date)
    gc(verbose=FALSE)

}
