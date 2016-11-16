# Find out which months failed to render.

library(lubridate)

Imagedir<-sprintf("%s/images/HadISST.2.2.daily.flat.fixed",Sys.getenv('SCRATCH'))

current.day<-ymd("1961-01-08")

end.day<-ymd("2012-01-01")

while(current.day<=end.day) {
  file.name<-sprintf("%s/%04d-%02d-%02d.png",
                     Imagedir,year(current.day),
                     month(current.day),day(current.day))
  if(!file.exists(file.name)) {
     print(file.name)
      sink('refill.step.slm')
      cat('#!/bin/ksh -l\n')
      cat('#SBATCH --output=/scratch/hadpb/slurm_output/HadISST2.2-%j.out\n')
      cat('#SBATCH --qos=normal\n')
      cat('#SBATCH --mem=5000\n')
      cat('#SBATCH --ntasks=1\n')
      cat('#SBATCH --ntasks-per-core=2\n')
      cat('#SBATCH --time=3\n')
         cat(sprintf("./HadISST_single.R --year=%d --month=%d --day=%d\n",
                     year(current.day),month(current.day),day(current.day)))
      sink()
      system('sbatch refill.step.slm')
  }
  current.day<-current.day+days(1)
}
