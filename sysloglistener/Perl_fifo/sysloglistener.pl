#!/usr/bin/perl -w

# Main Class
package Main;

# Imports
use strict;
use warnings;
use FindBin;
use Cwd;
use Sys::Syslog qw(:macros);
use lib "lib";
use Logger;

# Global variables
my $exec_name = "$FindBin::RealScript";
my $exec_path = "$FindBin::RealBin/$FindBin::RealScript";
my $exec_dir = "$FindBin::RealBin";
my $original_cwd = cwd();
my $identifier = $exec_name; $identifier =~ s/\.[^.]+$//;
my $fifo = defined($ENV{'XDG_RUNTIME_DIR'}) ? $ENV{'XDG_RUNTIME_DIR'} . "/$identifier.fifo" : "$identifier.fifo";

# Main
sub main {
    # Define subroutine variables
    my $exit_code = 0;
    my $pid;
    my $msg;
    my $status = 0;

    # Test the logger module
    Logger::set_fifo($fifo);
    #    Logger::set_log_file($identifier . ".log");
    Logger::set_identifier($identifier);
    Logger::listen();

    Logger::send(MESSAGE => "Test message!");
    Logger::send(LOG_LEVEL => LOG_CRIT, MESSAGE => "2B is amazing!");
    
    Logger::reset();

    # End subroutine
    return $exit_code;
}

# End Main Class
1;

# Execute
exit(Main::main());

