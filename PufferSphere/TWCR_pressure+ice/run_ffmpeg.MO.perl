#!/usr/bin/perl

# Make a video from the TWCR spherical images

use strict;
use warnings;

# Put the selected images in a temporary directory
my $Tdir = "/data/local/hadpb/images/ffmpeg/$$";
mkdir($Tdir) or die "Couldn't make $Tdir";

my $Count = 0;
my $Glob  = "/data/local/hadpb/images/P+I/*.png";
foreach my $ImageFile ( glob($Glob) ) {
    unless ( -r $ImageFile ) { die "Missing image $ImageFile"; }
     my $Nfname = sprintf "%s/%04d.png", $Tdir, $Count++;
    unless( -r  $Nfname) { `convert -gamma 0.6 $ImageFile $Nfname`; }
}

`ffmpeg -qscale 3 -r 24 -i $Tdir/%04d.png /data/local/hadpb/images/P+I.mov`;

#`rm -r $Tdir`;
