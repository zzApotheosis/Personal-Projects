#!/bin/perl

package Main;


use strict;
use warnings;

sub main {
        my $exit_code = 0;
        my @test_libs = ('addition', 'subtraction', 'multiplication',
                'division', 'modulo', 'factorial');
        
        for (my $i = 0; $i < 3; $i++) {
                for (my $j = 0; $j < scalar(@test_libs); $j++) {
                        system("cp -rfv test-addition-00.c test-$test_libs[$j]-" . sprintf("%02d", $i) . ".c");
                }
        }
        

        return $exit_code;
}

1;

exit(Main::main());
