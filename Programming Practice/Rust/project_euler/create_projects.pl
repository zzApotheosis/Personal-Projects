#!/usr/bin/perl -w

# Imports
use strict;
use warnings;

# Main
sub main {
    # Define method variables
    my $exit_code = 0;
    
    # Iterate
    for (my $i = 1; $i <= 1000; $i++) {
        system("cargo new problem_" . sprintf("%04d", $i));
    }
    
    # Done
    return $exit_code;
}

# Execute
exit main();

