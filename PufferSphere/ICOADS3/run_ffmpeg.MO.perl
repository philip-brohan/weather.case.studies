#!/usr/bin/perl

# Make a video from the TWCR spherical images

use strict;
use warnings;

# Put the selected images in a temporary directory
my $Tdir = "/data/local/hadpb/images/ffmpeg/$$";
mkdir($Tdir) or die "Couldn't make $Tdir";

my $Count = 0;
my $Glob  = "/data/local/hadpb/images/icoads_3.0/*.png";
foreach my $ImageFile ( glob($Glob) ) {
    unless ( -r $ImageFile ) { die "Missing image $ImageFile"; }
     my $Nfname = sprintf "%s/%04d.png", $Tdir, $Count++;
    unless( -r  $Nfname) { `ln $ImageFile $Nfname`; }
}

`ffmpeg -qscale 3 -r 24 -i $Tdir/%04d.png /data/local/hadpb/images/ICOADS3.mov`;

`rm -r $Tdir`;
