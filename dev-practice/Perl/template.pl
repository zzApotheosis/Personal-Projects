#!/usr/bin/perl

# Main Class
package Main;

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
    STDERR->print("Script name: $script_name\n");
    STDERR->print("Script path: $script_path\n");
    STDERR->print("Script dir: $script_dir\n");
    STDERR->print("Original cwd: $original_cwd\n");
    
    # Done
    return $exit_code;
}

# End Main Class
1;

# Execute
exit(Main::main());
