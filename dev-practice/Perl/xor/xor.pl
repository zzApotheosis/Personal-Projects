#!/bin/env perl

package Main;

use strict;
use warnings;
use FindBin;
use Cwd;
use POSIX ();
use Getopt::Long;

use constant MODE_SPLIT => 1;
use constant MODE_JOIN => 2;

my $exec_name = "$FindBin::RealScript";
my $exec_path = "$FindBin::RealBin/$FindBin::RealScript";
my $exec_dir = "$FindBin::RealBin";
my $original_cwd = getcwd();
my $mode = 0;
my @inputs = ();
my @outputs = ();

sub main {
    my $exit_code = 0;

    Main::parse_args();

    Main::read();

    if ($mode == MODE_SPLIT) {
        $exit_code = Main::data_split();
    } elsif ($mode == MODE_JOIN) {
        $exit_code = Main::data_join();
    } else {
        die('Unknown mode');
    }

    Main::write();
    
    return $exit_code;
}

sub parse_args {
    my $help = 0;
    my @in = ();
    my @out = ();
    my $count = 0;
    
    GetOptions(
        'h|help' => \$help,
        'i|input=s' => \@in,
        'o|output=s' => \@out,
        'c|count=i' => \$count,
        );

    if ($help) {
        Main::print_help();
        exit 0;
    }

    if (scalar(@ARGV) < 1) {
        STDERR->write("No mode specified. Need 'split' or 'join'.\n");
        Main::print_help();
        exit 1;
    }
    
    {
        my $arg0 = shift(@ARGV);
        if ($arg0 eq 'split') {
            $mode = MODE_SPLIT;
        } elsif ($arg0 eq 'join') {
            $mode = MODE_JOIN;
        } else {
            STDERR->write("Unknown mode '$arg0'.\n");
            Main::print_help();
            exit 1;
        }
    }
    
    if (scalar(@in) == 0 && (scalar(@ARGV) == 0 && $mode != MODE_JOIN)) {
        STDERR->write("No input files specified.\n");
        Main::print_help();
        exit 1;
    }

    if (scalar(@out) > 0 && $count > 0) {
        STDERR->write("Cannot specify -o and -c together.\n");
        Main::print_help();
        exit 1;
    }

    if (scalar(@out) == 0 && $count == 0) {
        STDERR->write("No output files specified.\n");
        Main::print_help();
        exit 1;
    }

    my $extra_paths;
    $extra_paths = \@outputs if $mode == MODE_SPLIT;
    $extra_paths = \@inputs if $mode == MODE_JOIN;
    $extra_paths = \@inputs if $mode == MODE_SPLIT && $count > 0;

    while (scalar(@ARGV) > 0) {
        push(@$extra_paths, {path => shift(@ARGV), data => undef});
    }

    foreach (@in) {
        push(@inputs, {path => $_, data => undef});
    }

    foreach (@out) {
        push(@outputs, {path => $_, data => undef});
    }

    if ($mode == MODE_SPLIT) {
        my $digit_length = length($count);
        for (my $i = 0; $i < $count; $i++) {
            push(@outputs, {path => sprintf("%s.%0${digit_length}d", $inputs[0]->{path}, $i), data => undef});
        }
    }

    # Final check for inputs and outputs
    if (scalar(@inputs) == 0 || scalar(@outputs) == 0) {
        Main::print_help();
        exit 1;
    }
}

sub print_help {
    STDERR->write("Usage: $0 [split|join] [-h|--help] [-i|--input <file>] [-o|--output <file>]\n");
    STDERR->write("\n");
    STDERR->write("Arguments:\n");
    STDERR->write("    -h, --help              Show this help prompt and exit.\n");
    STDERR->write("    -i, --input <file>      Specify one or more input files.\n");
    STDERR->write("    -o, --output <file>     Specify one or more output files.\n");
    STDERR->write("    -c, --count <n>         Used in split operations only, without specifying --output.\n");
    STDERR->write("                            Splits the input given by --input into <n> outputs.\n");
}

sub read {
    foreach (@inputs) {
        $_->{data} = FileUtil::read($_->{path});
    }
}

sub data_split {
    # Check for satisfactory output count
    if (scalar(@outputs) < 2) {
        STDERR->write("Not enough outputs for split operation.\n");
        Main::print_help();
        exit 1;
    }

    # Check if we specified multiple input files (makes no sense for split operation)
    if (scalar(@inputs) > 1) {
        warn('Multiple input files specified; only using the first given input');
    }

    # Generate random data for these splits
    for (my $i = 0; $i < scalar(@outputs) - 1; $i++) {
        # Generate random data for this output
        $outputs[$i]->{data} = RNG::rand(length($inputs[0]->{data}));
    }

    # Apply XOR boolean logic to the last key split
    $outputs[scalar(@outputs) - 1]->{data} = $inputs[0]->{data};
    for (my $i = 0; $i < scalar(@outputs) - 1; $i++) {
        $outputs[scalar(@outputs) - 1]->{data} ^= $outputs[$i]->{data};
    }

    return 0;
}

sub data_join {
    # Check for satisfactory input count
    if (scalar(@inputs) < 2) {
        STDERR->write("Not enough inputs for join operation.\n");
        Main::print_help();
        exit 1;
    }

    # Check if we specified multiple output files (makes no sense for join operation)
    if (scalar(@outputs) > 1) {
        warn('Multiple output files specified; only using the first given output');
    }

    # Apply XOR boolean logic to derive the data
    $outputs[0]->{data} = $inputs[0]->{data};
    for (my $i = 1; $i < scalar(@inputs); $i++) {
        $outputs[0]->{data} ^= $inputs[$i]->{data};
    }

    return 0;
}

sub write {
    foreach (@outputs) {
        FileUtil::write($_->{path}, $_->{data});
    }
}

1;

package RNG;

use strict;
use warnings;

use constant RNG_ENDPOINT => '/dev/urandom';

sub rand {
    my $len = shift(@_);
    my $buffer;
    if (open(my $fh, '<', RNG->RNG_ENDPOINT)) {
        $fh->binmode();
        $fh->read($buffer, $len);
        $fh->close();
    } else {
        die($!);
    }
    return $buffer;
}

sub rand_array {
    my $len = shift(@_);
    my $buffer = RNG::rand($len);
    my @bytes = unpack("C*", $buffer);;
    return @bytes;
}

1;

package FileUtil;

use strict;
use warnings;

use constant BUF_SIZE => 256;

sub read {
    my $path = shift(@_);
    my $data;
    if (open(my $fh, '<', $path)) {
        while (!eof($fh)) {
            my $buffer = undef;
            my $bytes_read = $fh->read($buffer, FileUtil->BUF_SIZE);
            $data .= $buffer;
        }
        $fh->close();
    } else {
        die($!);
    }
    return $data;
}

sub write {
    my $path = shift(@_);
    my $data = shift(@_);
    if (open(my $fh, '>', $path)) {
        $fh->write($data);
        $fh->close();
    } else {
        die($!);
    }
}

1;

exit(Main::main());
