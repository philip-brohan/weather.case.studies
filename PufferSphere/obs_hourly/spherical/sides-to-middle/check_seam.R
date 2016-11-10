#!/usr/bin/Rscript --no-save

# Check an image longitude boundary conditions by folding it
#  sides to middle.

library(png)
library(getopt)

opt = getopt(c(
  'input',   'i', 2, "character",
  'output',  'm', 2, "character"
))

if ( is.null(opt$input) )   { stop("Input file not specified") }
if ( is.null(opt$output) )  { stop("Output file not specified") }

inf<-readPNG(opt$input)
bp<-as.integer(dim(inf)[2]/2)
opf<-inf
opf[,(bp+1):dim(inf)[2],]<-inf[,1:bp,]
opf[,1:bp,]<-inf[,(bp+1):dim(inf)[2],]
writePNG(opf,target=opt$output)


