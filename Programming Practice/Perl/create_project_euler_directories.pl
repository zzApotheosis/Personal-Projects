#!/usr/bin/perl

# Imports
use strict;
use warnings;
use FindBin;
use Cwd;
use File::Spec;

# Global variables
my $script_name = "$FindBin::RealScript";
my $script_path = "$FindBin::RealBin/$FindBin::RealScript";
my $script_dir = "$FindBin::RealBin";
my $original_cwd = getcwd();

# Main
sub main {
    # Define subroutine variables
    my $exit_code = 0;
    my $limit = 100;
    my $dir_prefix = "problem_";
    
    # Do code
    for (my $i = 0; $i < $limit; $i++) {
        mkdir(File::Spec->catfile($original_cwd, $dir_prefix . sprintf("%0" . length($limit) . "d", $i)));
    }
    
    # Done
    return $exit_code;
}

# Execute
exit(main());

