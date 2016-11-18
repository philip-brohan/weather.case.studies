# Find out which hours failed to render.

library(lubridate)

Imagedir<-sprintf("%s/images//ERA5_multivariate.blended",Sys.getenv('SCRATCH'))

current.day<-ymd("2016-01-03")
end.day<-ymd("2016-02-10")

while(current.day<=end.day) {
  for(hour in seq(0,23)) {
    for(i in c(0,25,50,75)) {
      file.name<-sprintf("%s/%04d-%02d-%02d:%02d.%02d.png",
                         Imagedir,year(current.day),
                         month(current.day),
                         day(current.day),hour,i)
      if(!file.exists(file.name)) {
        print(file.name)
      }
    }
   }
  current.day<-current.day+days(1)
}
