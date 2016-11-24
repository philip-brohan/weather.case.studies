# Find out which hours failed to render.

library(lubridate)

Imagedir<-sprintf("%s/images//ERA4v_multivariate.fractional.step",Sys.getenv('SCRATCH'))

current.day<-ymd("2016-01-02")
end.day<-ymd("2016-02-28")

while(current.day<=end.day) {
      for(hour in seq(0,23.75,0.25)) {
      file.name<-sprintf("%s/%04d-%02d-%02d:%02d.%02d.png",
                         Imagedir,year(current.day),
                         month(current.day),
                         day(current.day),as.integer(hour),
                         as.integer((hour%%1)*100))
      if(!file.exists(file.name)) {
        print(file.name)
      }
   }
  current.day<-current.day+days(1)
}
