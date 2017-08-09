#!/usr/bin/env Rscript

# Run a load of rendering jobs on SPICE - keeping no more than 1000
#  in the queue at once.

library(lubridate)

current.day<-ymd("2010-01-31")
end.day<-ymd("2010-12-31")

while(current.day<=end.day) {
  in.system<-system('squeue --user hadpb',intern=TRUE)
  n.new.jobs<-300-length(in.system)
  if(n.new.jobs>100) {
      for(hour in seq(0,23)) {
          sink('ERA5.multivariate.slm')
          cat('#!/bin/ksh -l\n')
          cat('#SBATCH --output=/scratch/hadpb/slurm_output/ERA5_multivariate.V3-%j.out\n')
          cat('#SBATCH --qos=normal\n')
          cat('#SBATCH --mem=50000\n')
          cat('#SBATCH --ntasks=4\n')
          cat('#SBATCH --ntasks-per-core=1\n')
          cat('#SBATCH --time=20\n')
          needed<-0
          fn<-NULL
           for(min in seq(0,0.75,0.25)) {
             fn<-sprintf("%s/images/ERA5_multivariate/%04d-%02d-%02d:%02d.%02d.png",
                         Sys.getenv('SCRATCH'),year(current.day),month(current.day),
                                              day(current.day),hour,as.integer(min*100))
             if(file.exists(fn) && file.info(fn)$size>0) next
             cat(sprintf("time ./full_single.R --year=%d --month=%d --day=%d --hour=%f &\n",
                         year(current.day),month(current.day),day(current.day),hour+min))
            needed<-needed+1
          }
          cat('wait\n')
          sink()
          if(needed>0) {
             system('sbatch ERA5.multivariate.slm')
           }
          unlink('ERA5.multivariate.slm')
      }
      current.day<-current.day+days(1)
  }
  if(current.day<=end.day) Sys.sleep(2)
}

