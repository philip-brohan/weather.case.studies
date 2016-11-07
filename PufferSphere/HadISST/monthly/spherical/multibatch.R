# Run a load of rendering jobs on SPICE - keeping no more than 1000
#  in the queue at once.

library(lubridate)

current.day<-ymd("1870-01-01")
#current.day<-ymd("1905-01-01")

end.day<-ymd("2012-01-01")
#end.day<-ymd("1907-01-01")

while(current.day<=end.day) {
  in.system<-system('squeue --user hadpb',intern=TRUE)
  n.new.jobs<-1000-length(in.system)
  if(n.new.jobs>30) {
     step<-1
     sink('multistart.step.slm')
      cat('#!/bin/ksh -l\n')
      cat('#SBATCH --output=/scratch/hadpb/slurm_output/HadISST-%j.out\n')
      cat('#SBATCH --qos=normal\n')
      cat('#SBATCH --mem=5000\n')
      cat('#SBATCH --ntasks=1\n')
      cat('#SBATCH --ntasks-per-core=1\n')
      cat('#SBATCH --time=25\n')
      this.job<-0
      while(step<=6) {
         cat(sprintf("./HadISST_single.R --year=%d --month=%d\n",
                     year(current.day),month(current.day)))
         current.day<-current.day+months(1)
         this.job<-this.job+1
         step<-step+1
      }
      sink()
      system('sbatch multistart.step.slm')
     sink('multistart.step.slm')
      cat('#!/bin/ksh -l\n')
      cat('#SBATCH --output=/scratch/hadpb/slurm_output/HadISST-%j.out\n')
      cat('#SBATCH --qos=normal\n')
      cat('#SBATCH --mem=5000\n')
      cat('#SBATCH --ntasks=1\n')
      cat('#SBATCH --ntasks-per-core=1\n')
      cat('#SBATCH --time=25\n')
      this.job<-0
      while(step<=12) {
         cat(sprintf("./HadISST_single.R --year=%d --month=%d\n",
                     year(current.day),month(current.day)))
         current.day<-current.day+months(1)
         this.job<-this.job+1
         step<-step+1
      }
      sink()
      system('sbatch multistart.step.slm')      
  }
  if(current.day<=end.day) Sys.sleep(1)
}
