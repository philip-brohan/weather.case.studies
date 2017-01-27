# Run a hourly ICOADS render task on SPICE

library(lubridate)

current.day<-ymd("1978-01-01")
end.day<-ymd("1978-12-31")

peak.no.jobs<-500

while(current.day<=end.day) {
  in.system<-system('squeue --user hadpb',intern=TRUE)
  n.new.jobs<-peak.no.jobs-length(in.system)
  while(n.new.jobs<8) {
   Sys.sleep(10)
   in.system<-system('squeue --user hadpb',intern=TRUE)
   n.new.jobs<-peak.no.jobs-length(in.system)
  }
  for(hour in c(0,3,6,9,12,18,21)) {
      sink('ICOADS3.hourly.slm')
      cat('#!/bin/ksh -l\n')
      cat('#SBATCH --output=/scratch/hadpb/slurm_output/ICOADS3.hourly-%j.out\n')
      cat('#SBATCH --qos=normal\n')
      cat('#SBATCH --mem=5000\n')
      cat('#SBATCH --ntasks=1\n')
      cat('#SBATCH --ntasks-per-core=2\n')
      cat('#SBATCH --time=10\n')
      cat('export R_LIBS=/home/h03/hadpb/R/packages:/project/ukmo/rhel6/R/R-3.2.2/library/\n')
      cat(sprintf("./ICOADS_3_single.R --year=%d --month=%d --day=%d --hour=%d\n",
                     year(current.day),month(current.day),day(current.day),hour))
      sink()
      system('sbatch ICOADS3.hourly.slm')
      unlink('ICOADS3.hourly.slm')
   }
   current.day<-current.day+days(1)
}
