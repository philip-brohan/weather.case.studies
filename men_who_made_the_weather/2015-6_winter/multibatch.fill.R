# Run a load of rendering jobs on SPICE - keeping no more than 1000
#  in the queue at once.
# Repeat failed calculations.

library(lubridate)

Imagedir<-sprintf("%s/images/UK_winter_2015",Sys.getenv('SCRATCH'))

current.day<-ymd("2015-11-02")
end.day<-ymd("2016-02-28")

while(current.day<=end.day) {
     for(hour in seq(0,23.75,0.25)) {
        
      file.name<-sprintf("%s/%04d-%02d-%02d:%02d.%02d.png",
                         Imagedir,year(current.day),
                         month(current.day),
                         day(current.day),as.integer(hour),
                         as.integer(hour%%1*100))
      if(!file.exists(file.name) || file.info(file.name)$size==0) {
          print(file.name)
          sink('multistart.step.slm')
          cat('#!/bin/ksh -l\n')
          cat('#SBATCH --output=/scratch/hadpb/slurm_output/UK_2015_winter-%j.out\n')
          cat('#SBATCH --qos=normal\n')
          cat('#SBATCH --mem=5000\n')
          cat('#SBATCH --ntasks=1\n')
          cat('#SBATCH --ntasks-per-core=2\n')
          cat('#SBATCH --time=5\n')
             cat(sprintf("./full_single.R --year=%d --month=%d --day=%d --hour=%f\n",
                         year(current.day),month(current.day),day(current.day),hour))
          sink()
          system('sbatch multistart.step.slm')
      }
    }
  current.day<-current.day+days(1)
  while(length(system('squeue --user hadpb',intern=TRUE))>900) Sys.sleep(10)
}
