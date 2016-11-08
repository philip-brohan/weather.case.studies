# Find out which hours failed to render.

library(lubridate)

Imagedir<-sprintf("%s/images/P+I_rotating_1916",Sys.getenv('SCRATCH'))

current.day<-ymd("1916-01-01")
end.day<-ymd("1916-12-31")

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
