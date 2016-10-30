# Find out which months failed to render.

library(lubridate)

Imagedir<-sprintf("%s/images/HadISST.2.1.red_blue",Sys.getenv('SCRATCH'))

current.day<-ymd("1870-01-01")

end.day<-ymd("2012-01-01")

while(current.day<=end.day) {
  idx<-3
  file.name<-sprintf("%s/%04d-%02d.%02d.png",
                     Imagedir,year(current.day),
                     month(current.day),idx)
  if(!file.exists(file.name)) {
    print(file.name)
  }
  current.day<-current.day+months(1)
}
