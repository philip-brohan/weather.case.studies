for month in {1..12} 
do

echo '#!/bin/ksh -l\n' > mkc.slm
echo '#SBATCH --output=/scratch/hadpb/slurm_output/ERA5_clim-%j.out\n' >> mkc.slm
echo '#SBATCH --qos=normal\n' >> mkc.slm
echo '#SBATCH --mem=32G\n' >> mkc.slm
echo '#SBATCH --ntasks=1\n' >> mkc.slm
echo '#SBATCH --ntasks-per-core=2\n' >> mkc.slm
echo '#SBATCH --time=30\n' >> mkc.slm
echo './make.ERA5.climatology.for.month.R --variable=air.2m --month=$month'  >> mkc.slm
sbatch mkc.slm
rm mkc.slm

echo '#!/bin/ksh -l\n' > mkc.slm
echo '#SBATCH --output=/scratch/hadpb/slurm_output/ERA5_clim-%j.out\n' >> mkc.slm
echo '#SBATCH --qos=normal\n' >> mkc.slm
echo '#SBATCH --mem=32G\n' >> mkc.slm
echo '#SBATCH --ntasks=1\n' >> mkc.slm
echo '#SBATCH --ntasks-per-core=2\n' >> mkc.slm
echo '#SBATCH --time=30\n' >> mkc.slm
echo './make.ERA5.climatology.for.month.R --variable=prmsl --month=$month' >> mkc.slm
sbatch mkc.slm
rm mkc.slm

done
