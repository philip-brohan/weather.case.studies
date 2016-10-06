# Run a load of rendering jobs on SPICE - keeping no more than 1000
#  in the queue at once.

current.step<-0

# How many steps in total
limit<-8712
# How may steps in one job
#  (SPICE does not like thousands of very short jobs)
per.job<-5

while(current.step<limit) {
  in.system<-system('squeue --user hadpb',intern=TRUE)
  n.new.jobs<-1000-length(in.system)
  if(n.new.jobs>0) {
    min.step<-current.step+1
    max.step<-min(current.step+n.new.jobs,limit)
    step<-min.step
    while(step<=max.step) {
      sink('multistart.step.slm')
      cat('#!/bin/ksh -l\n')
      cat('#SBATCH --output=/scratch/hadpb/slurm_output/P+I-%j.out\n')
      cat('#SBATCH --qos=normal\n')
      cat('#SBATCH --mem=5000\n')
      cat('#SBATCH --ntasks=1\n')
      cat('#SBATCH --ntasks-per-core=2\n')
      cat('#SBATCH --time=5\n')
      while(step<=max.step && step<=min.step+per.job) {
         cat(sprintf("./P+I_rotate.R --step=%d\n",step))
         step<-step+1
      }
      min.step<-min.step+per.job
      sink()
      system('sbatch multistart.step.slm')
    }
    current.step<-max.step
  }
  if(current.step<limit) Sys.sleep(10)
}
