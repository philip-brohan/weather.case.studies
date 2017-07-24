#!/usr/bin/env Rscript

# Download all the ERA5 data needed to make climatologies

library(GSDF.ERA5)

for(year in seq(2010,2016)) {
  for(month in seq(1,12)) {
    for(var in c('prmsl','air.2m')) {
       ERA5.fetch.data.for.month(var,year,month)
     }
  }
}
