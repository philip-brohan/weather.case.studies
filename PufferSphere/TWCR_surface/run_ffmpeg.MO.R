# For the Puffersphere, need to apply major gamma correction to the
#  images and them convert them to .mov
#
# As the MO version of ffmpeg is so ancient, need 2 steps and
#  intermediate files.
#
# Run the gamma corrections in parallel - slow.

library(parallel)

Tdir<-"/data/local/hadpb/images/ffmpeg/"
if(!file.exists(Tdir)) dir.create(Tdir,recursive=TRUE)

Glob<-Sys.glob("/data/local/hadpb/images/TWCR_3.5.1_spherical_obliquity/2014/*.png")

conv.file<-function(i) {
  Image.file<-Glob[i]
  if(!file.exists(Image.file)) stop(sprintf("Missing file %s",
                                   Image.file))
  nfname<-sprintf("%s/%04d.png",Tdir,i)
  if(file.exists(nfname)) return()
  system(sprintf("convert -gamma 0.6 %s %s",Image.file,nfname))
}

mclapply(seq(1,length(Glob)),conv.file,mc.cores=6)

system(sprintf("ffmpeg -qscale 3 -r 24 -i %s/%%04d.png /data/local/hadpb/images/TWCR_2014_spherical_obliquity.mov",Tdir))


                                    
                                 
  
