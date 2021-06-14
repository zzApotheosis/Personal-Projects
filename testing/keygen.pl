#!/usr/bin/perl -w

# Imports
use strict;
use warnings;
use FindBin;
use Cwd;

# Global variables
my $exec_name = "$FindBin::RealScript";
my $exec_path = "$FindBin::RealBin/$FindBin::RealScript";
my $exec_dir = "$FindBin::RealBin";
my $original_cwd = cwd();

# Main Class
package Main;

# Main Subroutine
sub main {
    # Define subroutine variables
    my $exit_code = 0;
    
    # Done
    return($exit_code);
}

# End Main Class
1;

# Execute
exit(Main::main());

