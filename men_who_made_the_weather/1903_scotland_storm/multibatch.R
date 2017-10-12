# Run a load of rendering jobs on SPICE - keeping no more than 1000
#  in the queue at once.

library(lubridate)

current.day<-ymd("1903-02-20")
end.day<-ymd("1903-02-28")

while(current.day<=end.day) {
  in.system<-system('squeue --user hadpb',intern=TRUE)
  n.new.jobs<-1000-length(in.system)
  if(n.new.jobs>100) {
      for(hour in seq(0,23.75,0.25)) {
          sink('multistart.step.slm')
          cat('#!/bin/ksh -l\n')
          cat('#SBATCH --output=/scratch/hadpb/slurm_output/1894_scotland-%j.out\n')
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
      current.day<-current.day+days(1)
  }
  if(current.day<=end.day) Sys.sleep(2)
}
