#!/usr/bin/perl -w

# Execute this script with: valgrind ./memleak.pl

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
    
    # Do code
    print(STDOUT "Hi\n");

    # Done
    return $exit_code;
}

# Execute
exit(main());

