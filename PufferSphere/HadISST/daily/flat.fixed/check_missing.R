# Find out which months failed to render.

library(lubridate)

Imagedir<-sprintf("%s/images/HadISST.2.2.daily.flat.fixed",Sys.getenv('SCRATCH'))

current.day<-ymd("1961-01-01")

end.day<-ymd("2012-01-01")

while(current.day<=end.day) {
  file.name<-sprintf("%s/%04d-%02d-%02d.png",
                     Imagedir,year(current.day),
                     month(current.day),day(current.day))
  if(!file.exists(file.name)) {
    print(file.name)
  }
  current.day<-current.day+days(1)
}
