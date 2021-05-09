#!/usr/bin/perl -w

# Main Class
package Main;

# Imports
use strict;
use warnings;
use FindBin;
use Cwd;
use Sys::Syslog qw(:macros);
use lib 'lib';
use LogUtil;

# Class fields
my $exec_name = "$FindBin::RealScript";
my $exec_path = "$FindBin::RealBin/$FindBin::RealScript";
my $exec_dir = "$FindBin::RealBin";
my $original_cwd = cwd();
my $identifier = $exec_name; $identifier =~ s/\.[^.]*$//;
my $socket = defined($ENV{'XDG_RUNTIME_DIR'}) ? $ENV{'XDG_RUNTIME_DIR'} . "/S.$identifier" : "$exec_dir/S.$identifier";

# Main subroutine
sub main {
    # Define subroutine variables
    my $exit_code = 0;

    LogUtil::set_socket($socket);
    LogUtil::set_identifier($identifier);
    LogUtil::listen();

    system("./example_subscript.pl");

    while (1) {
        my $msg = <STDIN>;
        last if (!defined($msg));
        chomp($msg);
        LogUtil::send(MESSAGE => $msg);
    }

    LogUtil::reset();

    # End main subroutine
    return $exit_code;
}

# End Main Class
1;

# Execute
exit(Main::main());
