for month in {1..12} 
do

echo '#!/bin/ksh -l' > mkc.slm
echo '#SBATCH --output=/scratch/hadpb/slurm_output/ERA5_clim-%j.out' >> mkc.slm
echo '#SBATCH --qos=normal' >> mkc.slm
echo '#SBATCH --mem=32G' >> mkc.slm
echo '#SBATCH --ntasks=1' >> mkc.slm
echo '#SBATCH --ntasks-per-core=2' >> mkc.slm
echo '#SBATCH --time=2:30:00' >> mkc.slm
echo "./make.ERA5.climatology.for.month.R --variable=air.2m --month=$month"  >> mkc.slm
sbatch mkc.slm
rm mkc.slm

echo '#!/bin/ksh -l' > mkc.slm
echo '#SBATCH --output=/scratch/hadpb/slurm_output/ERA5_clim-%j.out' >> mkc.slm
echo '#SBATCH --qos=normal' >> mkc.slm
echo '#SBATCH --mem=32G' >> mkc.slm
echo '#SBATCH --ntasks=1' >> mkc.slm
echo '#SBATCH --ntasks-per-core=2' >> mkc.slm
echo '#SBATCH --time=2:30:00' >> mkc.slm
echo "./make.ERA5.climatology.for.month.R --variable=prmsl --month=$month" >> mkc.slm
sbatch mkc.slm
rm mkc.slm

done
