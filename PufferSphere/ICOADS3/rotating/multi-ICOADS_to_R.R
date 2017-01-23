
for(year in seq(1800,1950)) {
  for(month in seq(1,12)) {
  print(sprintf("%d %d",year,month))
    system(sprintf("./ICOADS_to_R.R --year=%d --month=%d",year,month))
  }
}
