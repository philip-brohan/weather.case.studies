# Run a daily ICOADS render task on SPICE

library(lubridate)

current.day<-ymd("1800-01-01")
end.day<-ymd("2004-12-30")

peak.no.jobs<-500

while(current.day<=end.day) {
  in.system<-system('squeue --user hadpb',intern=TRUE)
  n.new.jobs<-peak.no.jobs-length(in.system)
  while(n.new.jobs<1) {
   Sys.sleep(10)
   in.system<-system('squeue --user hadpb',intern=TRUE)
   n.new.jobs<-peak.no.jobs-length(in.system)
  }
  sink('ICOADS3.daily.slm')
  cat('#!/bin/ksh -l\n')
  cat('#SBATCH --output=/scratch/hadpb/slurm_output/ICOADS3-%j.out\n')
  cat('#SBATCH --qos=normal\n')
  cat('#SBATCH --mem=5000\n')
  cat('#SBATCH --ntasks=1\n')
  cat('#SBATCH --ntasks-per-core=2\n')
  cat('#SBATCH --time=10\n')
  # Fast for early years, so several days/job
  per.job<-max(1,as.integer((1980-year(current.day))/5))
  this.job<-0
  while(this.job<per.job) {
     cat(sprintf("./ICOADS_3_single.R --year=%d --month=%d --day=%d\n",
                 year(current.day),month(current.day),day(current.day)))
     current.day<-current.day+days(1)
     this.job<-this.job+1
     if(current.day>end.day) break
  }
  sink()
  system('sbatch ICOADS3.daily.slm')
  unlink('sbatch ICOADS3.daily.slm')
}
