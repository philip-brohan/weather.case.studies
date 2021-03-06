# Make the de-flickered sperical wind video with orange wind arrows

dates<-substr(basename(list.files("/scratch/hadpb/images/TWCR_spherical_obliquity_spice",
                                  pattern='rd')),13,28)

current.step<-0
limit<-length(dates)
max.current<-900
num.simultanious<-50

while(current.step<limit) {
  in.system<-system('squeue --user hadpb',intern=TRUE)
  n.new.jobs<-max.current-length(in.system)
  if(n.new.jobs>0) {
    min.step<-current.step+1
    max.step<-min(current.step+n.new.jobs*num.simultanious,limit)
    step<-min.step
    while(step <= max.step) {
      sink('multistart.step.slm')
      cat('#!/bin/ksh -l\n')
      cat('#SBATCH --qos=normal\n')
      cat('#SBATCH --output=/scratch/hadpb/slurm_output_orange/%j.out\n')
      cat('#SBATCH --mem=10000\n')
      cat('#SBATCH --ntasks=1\n')
      cat('#SBATCH --ntasks-per-core=2\n')
      cat('#SBATCH --time=30\n')
      count<-1
      while(step<=max.step && count<=num.simultanious) {
         cat(sprintf("./make_year_plots_single_orange.R --date=%s\n",dates[step]))
         count<-count+1
         step<-step+1
      }              
      sink()
      system('sbatch multistart.step.slm')
    }
    current.step<-current.step+n.new.jobs
    Sys.sleep(5)
  }
  if(current.step<limit) Sys.sleep(50)
}
