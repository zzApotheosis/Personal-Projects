#!/usr/bin/perl

# Imports
use strict;
use warnings;
use FindBin;
use Cwd;
use Math::BigInt;

# Global variables
my $script_name = "$FindBin::RealScript";
my $script_path = "$FindBin::RealBin/$FindBin::RealScript";
my $script_dir = "$FindBin::RealBin";
my $original_cwd = getcwd();

# Main
sub main {
    # Define subroutine variables
    my $exit_code = 0;
    my $base = 2;
    my $power = 1000;
    my $number = Math::BigInt->new($base)->bpow($power);
    my $sum = 0;
    
    # Calculate sum of digits
    foreach (split(//, $number)) {
        $sum += $_;
    }

    # Print results
    print(STDOUT "$sum\n");
    
    # Done
    return $exit_code;
}

# Execute
exit(main());

