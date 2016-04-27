# For the Puffersphere, need to apply major gamma correction to the
#  images and them convert them to .mov
#
# Run the gamma corrections in parallel - slow.

library(parallel)

Tdir<-"/Users/philip/LocalData/images/ffmpeg/"
if(!file.exists(Tdir)) dir.create(Tdir,recursive=TRUE)

Glob<-Sys.glob("/Users/philip/LocalData/images/TWCR_3.5.1_spherical_obliquity/2014/*.png")

conv.file<-function(i) {
  Image.file<-Glob[i]
  if(!file.exists(Image.file)) stop(sprintf("Missing file %s",
                                   Image.file))
  nfname<-sprintf("%s/%05d.png",Tdir,i)
  if(file.exists(nfname)) return()
  system(sprintf("convert -gamma 0.6 %s %s",Image.file,nfname))
  if(i<36) {
    frac<-as.integer((1-i/36)*100)
    system(sprintf("mogrify -fill black -colorize %d%% %s",frac,nfname))
    }
  if(length(Glob)-i<36) {
    frac<-as.integer((1-(length(Glob)-i)/36)*100)
    system(sprintf("mogrify -fill black -colorize %d%% %s",frac,nfname))
    }
}

mclapply(seq(1,length(Glob)),conv.file,mc.cores=8)

system("ffmpeg -r 36 -pattern_type glob -i /Users/philip/LocalData/images/ffmpeg/\\*.png -c:v libx264 -threads 8 -preset slow -tune animation -profile:v high -level 4.2 -pix_fmt yuv420p -crf 22 -c:a copy /Users/philip/LocalData/images/TWCR_2014_full_year.mov")

