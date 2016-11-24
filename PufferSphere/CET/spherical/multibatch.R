# Run a load of rendering jobs on SPICE - keeping no more than 1000
#  in the queue at once.

library(lubridate)

current.day<-ymd("1659-01-01")

end.day<-ymd("2016-09-01")
# How may steps in one job
#  (SPICE does not like thousands of very short jobs)
per.job<-12

while(current.day<=end.day) {
  in.system<-system('squeue --user hadpb',intern=TRUE)
  n.new.jobs<-1000-length(in.system)
  if(n.new.jobs>30) {
    step<-1
    while(step<=12) {
      sink('multistart.step.slm')
      cat('#!/bin/ksh -l\n')
      cat('#SBATCH --output=/scratch/hadpb/slurm_output/CET-%j.out\n')
      cat('#SBATCH --qos=normal\n')
      cat('#SBATCH --mem=5000\n')
      cat('#SBATCH --ntasks=1\n')
      cat('#SBATCH --ntasks-per-core=2\n')
      cat('#SBATCH --time=10\n')
      this.job<-0
      while(step<=12 && this.job<per.job) {
         for(frame in seq(1,3)) {
            cat(sprintf("./plot.single.R --year=%d --month=%d --frame=%d\n",
                        year(current.day),month(current.day),frame))
          }
         current.day<-current.day+months(1)
         this.job<-this.job+1
         step<-step+1
      }
      sink()
      system('sbatch multistart.step.slm')
    }
  }
  if(current.day<=end.day) Sys.sleep(1)
}
