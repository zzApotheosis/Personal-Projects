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

    # DO TEST
    LogUtil::set_identifier($identifier);
    LogUtil::set_socket($socket);

    # LogUtil::listen();

    LogUtil::send(MESSAGE => "Hello, world!");

    LogUtil::set_log_file("$identifier.log");
    LogUtil::send(LOG_LEVEL => LOG_NOTICE, MESSAGE => "2B is amazing!");
    LogUtil::set_log_file(undef);
    LogUtil::send(LOG_LEVEL => LOG_EMERG, MESSAGE => "FATAL");
    LogUtil::send(MESSAGE => "SYSLOG_LISTENER_SOCK=" . $ENV{'SYSLOG_LISTENER_SOCK'});

    while (1) {
        my $msg = <STDIN>;
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
