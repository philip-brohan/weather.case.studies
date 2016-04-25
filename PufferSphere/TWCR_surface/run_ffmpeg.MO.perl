#!/usr/bin/perl

# Make a video from the TWCR spherical images

use strict;
use warnings;

# Put the selected images in a temporary directory
my $Tdir = $ENV{TMPDIR};

my $Count = 0;
my $Glob  = "/scratch/hadpb/images/TWCR_spherical_obliquity_spice/*.png";
foreach my $ImageFile ( glob($Glob) ) {
    unless ( -r $ImageFile ) { die "Missing image $ImageFile"; }
     my $Nfname = sprintf "%s/%05d.png", $Tdir, $Count++;
    unless( -r  $Nfname) { `cp $ImageFile $Nfname`; }
}

# Fade in and out
for(my $im=0;$im<24;$im++) {
  my $Nfname = sprintf "%s/%05d.png", $Tdir, $im;
  my $Fraction=sprintf "%d",(1-$im/24)*100;
  `mogrify -fill black -colorize $Fraction% $Nfname`;
}
for(my $im=$Count-24;$im<=$Count;$im++) {
  my $Nfname = sprintf "%s/%05d.png", $Tdir, $im;
  my $Fraction=sprintf "%d",(1-($Count-$im)/24)*100;
  `mogrify -fill black -colorize $Fraction% $Nfname`;
}

unlink('/scratch/hadpb/images/TWCR_new_colours.mov');

`ffmpeg -r 36 -i $Tdir/%05d.png -vcodec libx264 -vpre slow -crf 22
/scratch/hadpb/images/TWCR_new_colours.mov`;

