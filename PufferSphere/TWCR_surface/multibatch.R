# Fill up spice with 20CR rendering jobs

dates<-substr(basename(list.files("/scratch/hadpb/images/2014")),13,28)

current.step<-0
limit<-length(dates)

while(current.step<limit) {
  in.system<-system('squeue --user hadpb',intern=TRUE)
  n.new.jobs<-1000-length(in.system)
  if(n.new.jobs>0) {
    min.step<-current.step+1
    max.step<-min(current.step+n.new.jobs,limit)
    for(step in seq(min.step,max.step)) {
      sink('multistart.step.slm')
      cat('#!/bin/ksh -l\n')
      cat('#SBATCH --qos=normal\n')
      cat('#SBATCH --mem=5000\n')
      cat('#SBATCH --ntasks=1\n')
      cat('#SBATCH --ntasks-per-core=2\n')
      cat('#SBATCH --time=5\n')
      cat(sprintf("./make_year_plots_single.R --date=%s\n",dates[step]))
      sink()
      system('sbatch multistart.step.slm')
    }
    current.step<-current.step+n.new.jobs
  }
  if(current.step<limit) Sys.sleep(10)
}
