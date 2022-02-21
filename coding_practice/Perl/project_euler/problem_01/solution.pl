#!/usr/bin/perl

# Imports
use strict;
use warnings;
use FindBin;
use Cwd;

# Global variables
my $script_name = "$FindBin::RealScript";
my $script_path = "$FindBin::RealBin/$FindBin::RealScript";
my $script_dir = "$FindBin::RealBin";
my $original_cwd = getcwd();

# Main
sub main {
    # Define subroutine variables
    my $exit_code = 0;
    my $limit = 1000;
    my $sum = 0;
    
    # Do code
    for (my $i = 0; $i < $limit; $i++) {
        if ($i % 3 == 0 || $i % 5 == 0) {
            $sum += $i;
        }
    }

    # Print results
    print(STDOUT "$sum\n");
    
    # Done
    return $exit_code;
}

# Execute
exit(main());

