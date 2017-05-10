# Parallelise the climatology calculation on SPICE

library(lubridate)

for(hour in seq(1,23)) {
     sink('multistart.step.slm')
      cat('#!/bin/ksh -l\n')
      cat('#SBATCH --output=/scratch/hadpb/slurm_output/ERA5_climatology-%j.out\n')
      cat('#SBATCH --qos=normal\n')
      cat('#SBATCH --mem=5000\n')
      cat('#SBATCH --ntasks=1\n')
      cat('#SBATCH --ntasks-per-core=2\n')
      cat('#SBATCH --time=30\n')
         cat(sprintf("./ERA5_ERAI_climatology.R --hour=%d\n",hour))
      sink()
     system('sbatch multistart.step.slm')
   }

