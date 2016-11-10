# Run a load of rendering jobs on SPICE - keeping no more than 1000
#  in the queue at once.

library(lubridate)

current.day<-ymd("1961-01-01")

end.day<-ymd("2012-12-31")

while(current.day<=end.day) {
  in.system<-system('squeue --user hadpb',intern=TRUE)
  n.new.jobs<-1000-length(in.system)
  if(n.new.jobs>30) {
      sink('multistart.step.slm')
      cat('#!/bin/ksh -l\n')
      cat('#SBATCH --output=/scratch/hadpb/slurm_output/HadISST-%j.out\n')
      cat('#SBATCH --qos=normal\n')
      cat('#SBATCH --mem=5000\n')
      cat('#SBATCH --ntasks=1\n')
      cat('#SBATCH --ntasks-per-core=2\n')
      cat('#SBATCH --time=10\n')
      this.job<-0
      for(i in seq(1,31)) {
           in.file<-sprintf("%s/images/HadISST.2.2.daily.spherical/%04d-%02d-%02d.png",
                            Sys.getenv('SCRATCH'),year(current.day),
                            month(current.day),i)
           out.file<-sprintf("%s/images/HadISST.2.2.daily.spherical.stom/%04d-%02d.%02d.png",
                            Sys.getenv('SCRATCH'),year(current.day),
                            month(current.day),i)
         cat(sprintf("./check_seam.R --input=%s --output=%s\n",
                     in.file,out.file))
      }
         current.day<-current.day+months(1)
      sink()
      system('sbatch multistart.step.slm')
    }
  if(current.day<=end.day) Sys.sleep(2)
}
