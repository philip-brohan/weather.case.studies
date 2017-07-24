#!/usr/bin/env Rscript

# Download all the ERAI data needed to make ERA5 climatologies

library(GSDF.ERAI)

for(year in seq(1981,2016)) {
  for(month in seq(1,12)) {
    for(var in c('prmsl','air.2m')) {
       ERAI.fetch.data.for.month(var,year,month)
     }
  }
}
