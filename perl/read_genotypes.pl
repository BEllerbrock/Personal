#!/usr/bin/perl 

use warnings;
use strict;

my $file = 'genotypes.txt';
my $counter = 0;
open(FILE, $file) || die "Can't find file $file \n";

while (<FILE>) {
    chomp;
    $counter++;
    my($marker, $accession1, $accession2) = split('\t', $_);
    print "marker = $marker \n";
    $marker =~ s/^.{3}//;
    print "marker = $marker \n";
    print $marker . "\t" . $accession1 . "\t" . $accession2 ."   line number = $counter \n";  
    if ($counter == 3) { last;}
}
    
    

#&special_sort = {


#}
