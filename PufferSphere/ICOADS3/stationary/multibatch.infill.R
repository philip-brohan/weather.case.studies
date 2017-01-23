# Run a daily ICOADS render task on SPICE

library(lubridate)

current.day<-ymd("1800-01-01")
end.day<-ymd("2014-12-31")

peak.no.jobs<-250

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
  this.job<-0
  # Fast for early years, so several days/job
  per.job<-max(1,as.integer((1980-year(current.day))/5))
  while(this.job<per.job) {
     opfile<-sprintf("%s/images/ICOADS3.stationary/%04d-%02d-%02d.png",Sys.getenv('SCRATCH'),
                     year(current.day),month(current.day),day(current.day))
     if(!file.exists(opfile) || file.info(opfile)$size==0) {
        message(sprintf("%s\n",opfile))
        cat(sprintf("./ICOADS_3_single.R --year=%d --month=%d --day=%d\n",
                    year(current.day),month(current.day),day(current.day)))
        this.job<-this.job+1
     # }else {
     #   mode<-system(sprintf("identify %s",opfile),intern=TRUE)
     #   if(length(grep('Pseudo',mode))>0) {
     #      message(sprintf("%s is pseudo\n",opfile))
     #      unlink(opfile)
     #      cat(sprintf("./ICOADS_3_single.R --year=%d --month=%d --day=%d\n",
     #                  year(current.day),month(current.day),day(current.day)))
     #      this.job<-this.job+1
     #    }
      }
     current.day<-current.day+days(1)
     if(current.day>end.day) break
  }
  sink()
  system('sbatch ICOADS3.daily.slm')
  unlink('sbatch ICOADS3.daily.slm')
}
