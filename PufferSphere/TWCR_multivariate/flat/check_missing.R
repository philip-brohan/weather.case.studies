# Find out which hourss failed to render.

library(lubridate)

Imagedir<-sprintf("%s/images//TWCR_multivariate",Sys.getenv('SCRATCH'))

current.day<-ymd("2014-01-02")
end.day<-ymd("2014-12-30")

while(current.day<=end.day) {
  for(hour in seq(0,23)) {
      file.name<-sprintf("%s/%04d-%02d-%02d:%02d.png",
                         Imagedir,year(current.day),
                         month(current.day),
                         day(current.day),hour)
      if(!file.exists(file.name)) {
        print(file.name)
      }
   }
  current.day<-current.day+days(1)
}
