#!/usr/bin/env Rscript

# Run a load of rendering jobs on SPICE - keeping no more than 1000
#  in the queue at once.

library(lubridate)

current.day<-ymd("1918-01-01")
end.day<-ymd("1918-03-31")

while(current.day<=end.day) {
  in.system<-system('squeue --user hadpb',intern=TRUE)
  n.new.jobs<-300-length(in.system)
  if(n.new.jobs>100) {
      for(hour in seq(0,23)) {
          sink('V3vV2c_spaghetti.anomalies.slm')
          cat('#!/bin/ksh -l\n')
          cat('#SBATCH --output=/scratch/hadpb/slurm_output/TWCR_spaghetti.V3vV2c-%j.out\n')
          cat('#SBATCH --qos=normal\n')
          cat('#SBATCH --mem=20000\n')
          cat('#SBATCH --ntasks=4\n')
          cat('#SBATCH --ntasks-per-core=1\n')
          cat('#SBATCH --time=5\n')
          for(min in seq(0,0.75,0.25)) {
             cat(sprintf("time ./full_single.anomalies.R --year=%d --month=%d --day=%d --hour=%f &\n",
                         year(current.day),month(current.day),day(current.day),hour+min))
           }
          cat('wait \n')
          sink()
          system('sbatch V3vV2c_spaghetti.anomalies.slm')
          unlink('V3vV2c_spaghetti.anomalies.slm')
        }
      current.day<-current.day+days(1)
  }
  if(current.day<=end.day) Sys.sleep(2)
}
