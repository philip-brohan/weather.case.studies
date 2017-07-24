for month in {1..12} 
do
./make.V3.climatology.for.month.R --variable=air.2m --month=$month &
./make.V3.climatology.for.month.R --variable=prmsl --month=$month
done
