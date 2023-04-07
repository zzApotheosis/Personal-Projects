#!/usr/bin/perl -w

package Main;

use strict;
use warnings;
use FindBin;
use Cwd;

my $exec_name = "$FindBin::RealScript";
my $exec_path = "$FindBin::RealBin/$FindBin::RealScript";
my $exec_dir = "$FindBin::RealBin";
my $original_cwd = getcwd();

sub main {
    my $exit_code = 0;
    my $str;
    for (my $i = 1; $i <= 100; $i++) {
        $str = "";
        if ($i % 3 == 0) {
            $str .= "fizz";
        }
        if ($i % 5 == 0) {
            $str .= "buzz";
        }
        if (length($str) == 0) {
            $str .= $i;
        }
        STDOUT->printflush("$str\n");
    }
    return $exit_code;
}

1;

exit(Main::main());
