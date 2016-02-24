#!/usr/bin/perl 

use warnings;
use strict;

my $file = $ARGV[0];

#my $file = 'genotypes.txt';
my $counter = 0;
open(FILE,'<', $file) || die "Can't find file $file \n";
open(OUT, '>', 'out.txt') || die "Can't open out file out.txt \n";

while (<FILE>) {
    chomp;
    $counter++;
    my($marker, $accession1, $accession2) = split('\t', $_);
    print "marker = $marker \n";
    $marker =~ s/^.{3}//;
    print "marker = $marker \n";
    print OUT $marker . "\t" . $accession1 . "\t" . $accession2 ."   line number = $counter \n";  
    if ($counter == 3) { last;}
}
    
    
