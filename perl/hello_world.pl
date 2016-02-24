#!/usr/bin/perl

use warnings;
use strict;

my $statement = "Hello world \n";

print $statement;

my @numbers = qw/1 2 3 4 5/;

#my $last_numbeerr = $numbers[4];
#print $last_numrs, eber . "\n";

#push @numbers, 6;

#foreach my $number (@numbers) {
#    print $number;
#    print "\n";
#}

my @bryan_last_names = ['Ellerbrock', 'Bond'];

my %name_hash = ( 'Bryan' , @bryan_last_names,
		  'Guillaume' , 'Bauchet',
		  'Uba' , 'Ezenwanyi',
		  'Deborah' , 'Ade',
		  'Titima' , 'Tantikanjana');
my $first_name = 'Bryan';

#print $name_hash{$first_name} . "\n";


# unsorted
print "Here is the unsorted hash, order is not important\n";
for my $key (keys %name_hash) {
    print "first name: $key lastname: $name_hash{$key} \n";  
}

# sorted
print "Here we sort the keys before we loop so they are printed alphabetically\n";
for my $key (reverse sort keys %name_hash) {
    print "first name: $key lastname:$name_hash{$key} \n";  
}


