# Run a load of rendering jobs on SPICE - keeping no more than 1000
#  in the queue at once.

library(lubridate)

current.day<-ymd("1998-01-04")
#current.day<-ymd("1905-01-01")

end.day<-ymd("1998-01-30")
#end.day<-ymd("1907-01-01")
# How many steps in one job
#  (SPICE does not like thousands of very short jobs)
per.job<-5

while(current.day<=end.day) {
  in.system<-system('squeue --user hadpb',intern=TRUE)
  n.new.jobs<-1000-length(in.system)
  if(n.new.jobs>30) {
      sink('multistart.step.slm')
      cat('#!/bin/ksh -l\n')
      cat('#SBATCH --output=/scratch/hadpb/slurm_output/TWCR_assimilation-%j.out\n')
      cat('#SBATCH --qos=normal\n')
      cat('#SBATCH --mem=5000\n')
      cat('#SBATCH --ntasks=1\n')
      cat('#SBATCH --ntasks-per-core=2\n')
      cat('#SBATCH --time=10\n')

      in.files<-Sys.glob(sprintf("%s/images/1998-assimilation-rotating/%04d-%02d-%02d*.png",
                            Sys.getenv('SCRATCH'),year(current.day),
                            month(current.day),day(current.day)))
      out.files<-sub('rotating','rotating.stom',in.files)
      for(i in seq_along(in.files)) {
         cat(sprintf("./check_seam.R --input=%s --output=%s\n",
                     in.files[i],out.files[i]))
      }
      current.day<-current.day+days(1)
      sink()
      system('sbatch multistart.step.slm')
    }
  if(current.day<=end.day) Sys.sleep(1)
}
