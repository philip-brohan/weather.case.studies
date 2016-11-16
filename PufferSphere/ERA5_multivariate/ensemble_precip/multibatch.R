# Run a load of rendering jobs on SPICE - keeping no more than 1000
#  in the queue at once.

library(lubridate)

current.day<-ymd("2016-01-02")
end.day<-ymd("2016-02-28")

while(current.day<=end.day) {
  in.system<-system('squeue --user hadpb',intern=TRUE)
  n.new.jobs<-min(1000-length(in.system),as.integer(end.day-current.day)+1)
  if(n.new.jobs>0) {
    step<-0
    while(step<n.new.jobs-3) {
      sink('multistart.step.slm')
      cat('#!/bin/ksh -l\n')
      cat('#SBATCH --output=/scratch/hadpb/slurm_output/ERA5_multivariate-%j.out\n')
      cat('#SBATCH --qos=normal\n')
      cat('#SBATCH --mem=5000\n')
      cat('#SBATCH --ntasks=1\n')
      cat('#SBATCH --ntasks-per-core=2\n')
      cat('#SBATCH --time=15\n')
      for(hour in seq(0,5)) {
         cat(sprintf("./full_single.R --year=%d --month=%d --day=%d --hour=%d\n",
                     year(current.day),month(current.day),day(current.day),hour))
      }
      sink()
      step<-step+1
      system('sbatch multistart.step.slm')
      sink('multistart.step.slm')
      cat('#!/bin/ksh -l\n')
      cat('#SBATCH --output=/scratch/hadpb/slurm_output/ERA5_multivariate-%j.out\n')
      cat('#SBATCH --qos=normal\n')
      cat('#SBATCH --mem=5000\n')
      cat('#SBATCH --ntasks=1\n')
      cat('#SBATCH --ntasks-per-core=2\n')
      cat('#SBATCH --time=15\n')
      for(hour in seq(6,11)) {
         cat(sprintf("./full_single.R --year=%d --month=%d --day=%d --hour=%d\n",
                     year(current.day),month(current.day),day(current.day),hour))
      }
      sink()
      step<-step+1
      system('sbatch multistart.step.slm')
      sink('multistart.step.slm')
      cat('#!/bin/ksh -l\n')
      cat('#SBATCH --output=/scratch/hadpb/slurm_output/ERA5_multivariate-%j.out\n')
      cat('#SBATCH --qos=normal\n')
      cat('#SBATCH --mem=5000\n')
      cat('#SBATCH --ntasks=1\n')
      cat('#SBATCH --ntasks-per-core=2\n')
      cat('#SBATCH --time=15\n')
      for(hour in seq(12,17)) {
         cat(sprintf("./full_single.R --year=%d --month=%d --day=%d --hour=%d\n",
                     year(current.day),month(current.day),day(current.day),hour))
      }
      sink()
      step<-step+1
      system('sbatch multistart.step.slm')
      sink('multistart.step.slm')
      cat('#!/bin/ksh -l\n')
      cat('#SBATCH --output=/scratch/hadpb/slurm_output/ERA5_multivariate-%j.out\n')
      cat('#SBATCH --qos=normal\n')
      cat('#SBATCH --mem=5000\n')
      cat('#SBATCH --ntasks=1\n')
      cat('#SBATCH --ntasks-per-core=2\n')
      cat('#SBATCH --time=15\n')
      for(hour in seq(18,23)) {
         cat(sprintf("./full_single.R --year=%d --month=%d --day=%d --hour=%d\n",
                     year(current.day),month(current.day),day(current.day),hour))
      }
      sink()
      step<-step+1
      system('sbatch multistart.step.slm')
      current.day<-current.day+days(1)
    }
  }
  if(current.day<=end.day) Sys.sleep(2)
}
