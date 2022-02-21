#!/usr/bin/perl -w

# Main Class
package Main;

# Imports
use strict;
use warnings;
use FindBin;
use Cwd;
use MIME::Base64;

# Static Fields
my $exec_name = "$FindBin::RealScript";
my $exec_path = "$FindBin::RealBin/$FindBin::RealScript";
my $exec_dir = "$FindBin::RealBin";
my $original_cwd = getcwd();

# Main Subroutine
sub main {
    # Define subroutine variables
    my $status = 0;
    my $fh;
    my $binary_data;
    my @hex_data;
    my $base64_data;
    my $entropy_source = "/dev/urandom";
    my $binary_file = "test.bin";
    my $base64_file = "test.pem";

    # Read entropy source
    open($fh, '<:raw', $entropy_source) or die($!);
    $status = read($fh, $binary_data, 32);
    close($fh);
    $status = 0; # Reset status
    
    $base64_data = encode_base64($binary_data);
    chomp($base64_data);

    for (my $i = 0; $i < length($binary_data); $i++) {
        push(@hex_data, lc(sprintf('%02X', ord(substr($binary_data, $i, 1)))));
        STDOUT->printflush("0x$hex_data[$i]\n");
    }
    
    open($fh, '>', $binary_file) or die($!);
    $fh->printflush($binary_data);
    close($fh);

    open($fh, '>', $base64_file) or die($!);
    $fh->printflush("-----BEGIN-----\r\n");
    $fh->printflush("$base64_data\r\n");
    $fh->printflush("-----END-----\r\n");
    close($fh);

    # End main subroutine
    return($status);
}

# End Main Class
1;

# Execute
exit(Main::main());
