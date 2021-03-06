# Run a load of rendering jobs on SPICE - keeping no more than 1000
#  in the queue at once.

library(lubridate)

current.day<-ymd("2014-11-19")
end.day<-ymd("2014-12-31")

while(current.day<=end.day) {
  in.system<-system('squeue --user hadpb',intern=TRUE)
  n.new.jobs<-min(1000-length(in.system),as.integer(end.day-current.day)+1)
  if(n.new.jobs>0) {
    step<-0
    while(step<n.new.jobs) {
      sink('multistart.step.slm')
      cat('#!/bin/ksh -l\n')
      cat('#SBATCH --output=/scratch/hadpb/slurm_output/TWCR_multivariate_rotating-%j.out\n')
      cat('#SBATCH --qos=normal\n')
      cat('#SBATCH --mem=5000\n')
      cat('#SBATCH --ntasks=1\n')
      cat('#SBATCH --ntasks-per-core=2\n')
      cat('#SBATCH --time=20\n')
      for(hour in seq(0,23)) {
         cat(sprintf("./full_single.R --year=%d --month=%d --day=%d --hour=%d\n",
                     year(current.day),month(current.day),day(current.day),hour))
      }
      sink()
      current.day<-current.day+days(1)
      step<-step+1
      system('sbatch multistart.step.slm')
    }
  }
  if(current.day<=end.day) Sys.sleep(2)
}
