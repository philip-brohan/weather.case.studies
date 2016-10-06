# Run a load of rendering jobs on SPICE - keeping no more than 1000
#  in the queue at once.

library(lubridate)

current.day<-ymd("1998-01-04")

end.day<-ymd("1998-01-20")
# How may steps in one job
#  (SPICE does not like thousands of very short jobs)
per.job<-1

while(current.day<=end.day) {
  in.system<-system('squeue --user hadpb',intern=TRUE)
  n.new.jobs<-1000-length(in.system)
  if(n.new.jobs>30) {
    step<-1
    while(step<=28) {
      sink('multistart.step.slm')
      cat('#!/bin/ksh -l\n')
      cat('#SBATCH --output=/scratch/hadpb/slurm_output/Assimilation-%j.out\n')
      cat('#SBATCH --qos=normal\n')
      cat('#SBATCH --mem=5000\n')
      cat('#SBATCH --ntasks=1\n')
      cat('#SBATCH --ntasks-per-core=2\n')
      cat('#SBATCH --time=10\n')
      this.job<-0
      while(step<=28 && this.job<per.job) {
         cat(sprintf("./show_assimilation.R --year=%d --month=%d --day=%d --step=%d\n",
                     year(current.day),month(current.day),day(current.day),step))
         this.job<-this.job+1
         step<-step+1
      }
      sink()
      system('sbatch multistart.step.slm')
    }
    current.day<-current.day+days(1)
  }
  if(current.day<=end.day) Sys.sleep(10)
}
