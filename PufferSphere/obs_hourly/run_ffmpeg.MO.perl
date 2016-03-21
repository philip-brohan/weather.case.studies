#!/usr/bin/perl

# Make a video from the ISPD records

use strict;
use warnings;

# Put the selected images in a temporary directory
my $Tdir = "/data/local/hadpb/images/ffmpeg/$$";
mkdir($Tdir) or die "Couldn't make $Tdir";

my $Count = 0;
my $Glob  = "/data/local/hadpb/images/ISPD_by_pressure/2014/*.png";
foreach my $ImageFile ( glob($Glob) ) {
    unless ( -r $ImageFile ) { die "Missing image $ImageFile"; }
     my $Nfname = sprintf "%s/%04d.png", $Tdir, $Count++;
    unless( -r  $Nfname) { `cp $ImageFile $Nfname`; }
}

# Fade in and out
for(my $im=0;$im<24;$im++) {
  my $Nfname = sprintf "%s/%04d.png", $Tdir, $im;
  my $Fraction=sprintf "%d",(1-$im/24)*100;
  `mogrify -fill black -colorize $Fraction% $Nfname`;
}
for(my $im=$Count-24;$im<=$Count;$im++) {
  my $Nfname = sprintf "%s/%04d.png", $Tdir, $im;
  my $Fraction=sprintf "%d",(1-($Count-$im)/24)*100;
  `mogrify -fill black -colorize $Fraction% $Nfname`;
}
   
`ffmpeg -qscale 3 -r 24 -i $Tdir/%04d.png /data/local/hadpb/images/ISPD_by_pressure.mov`;

`rm -r $Tdir`;
