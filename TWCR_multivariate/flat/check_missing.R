# Find out which hours failed to render.

library(lubridate)

Imagedir<-sprintf("%s/images/TWCR_multivariate",Sys.getenv('SCRATCH'))

current.day<-ymd("1915-01-01")
end.day<-ymd("1915-02-28")

while(current.day<=end.day) {
  for(hour in seq(0,23)) {
    for(i in c(0,25,50,75)) {
      file.name<-sprintf("%s/%04d-%02d-%02d:%02d.%02d.png",
                         Imagedir,year(current.day),
                         month(current.day),
                         day(current.day),hour,i)
      if(!file.exists(file.name) || file.info(file.name)$size==0) {
        print(file.name)
      }
    }
   }
  current.day<-current.day+days(1)
}
