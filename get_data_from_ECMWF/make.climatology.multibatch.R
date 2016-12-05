# Parallelise the climatology calculation on SPICE

library(lubridate)

base.year<-1981
for(month in seq(1,12)) {
  dim<-days_in_month(ymd(sprintf("%04d-%02d-10",base.year,month)))
   for(day in seq(1,dim)) {
      sink('multistart.step.slm')
      cat('#!/bin/ksh -l\n')
      cat('#SBATCH --output=/scratch/hadpb/slurm_output/ERAI_climatology-%j.out\n')
      cat('#SBATCH --qos=normal\n')
      cat('#SBATCH --mem=5000\n')
      cat('#SBATCH --ntasks=1\n')
      cat('#SBATCH --ntasks-per-core=2\n')
      cat('#SBATCH --time=10\n')
      for(hour in seq(0,23)) {
         cat(sprintf("./ERAI_full_climatology.R --month=%d --day=%d --hour=%d\n",
                     month,day,hour))
      }
      sink()
     system('sbatch multistart.step.slm')
    }
  }
}
