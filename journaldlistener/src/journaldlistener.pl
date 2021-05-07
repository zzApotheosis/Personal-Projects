#!/usr/bin/perl -w

# Main Class
package Main;

# Imports
use strict;
use warnings;
use FindBin;
use Cwd;
use IO::Socket::UNIX;
use Time::HiRes qw(usleep);

# Global variables
my $exec_name = "$FindBin::RealScript";
my $exec_path = "$FindBin::RealBin/$FindBin::RealScript";
my $exec_dir = "$FindBin::RealBin";
my $original_cwd = cwd();
my $listener_socket_exec = "./a.out";
my $listener_socket = "socket";

# Main
sub main {
    # Define subroutine variables
    my $exit_code = 0;
    my $pid;
    my $client;
    my $msg;
    my $status = 0;

    # Fork and make child start Unix socket server
    $pid = fork();

    # Branch behavior
    if (!$pid) {
        if (! -x $listener_socket_exec && ! -e $listener_socket_exec) {
            die();
        }
        print(STDOUT "Starting Unix socket listener from Perl\n");
        exec($listener_socket_exec);
    }

    # Allow time to establish the Unix socket listener
    while (! -S $listener_socket) {
        $status++;
        if ($status > 10000) {
            kill('SIGINT', $pid);
            die("Cannot find $listener_socket");
        }
        usleep(10);
    }
    $status = 0;

    # Main Loop
    print(STDOUT "Begin main loop. Use \"exit\" to end program.\n");
    while (1) {
        chomp($msg = <STDIN>);
        last if $msg eq "exit";
        $client = IO::Socket::UNIX->new(Type => SOCK_STREAM(), Peer => $listener_socket);
        $client->send($msg);
        # $client->recv($msg, 100);
        # print(STDOUT $msg . "\n");
        $client->shutdown(SHUT_RDWR);
        $client->close();
    }

    # Clean up
    kill('SIGINT', $pid);

    # End subroutine
    return $exit_code;
}

# End Main Class
1;

# Execute
exit(Main::main());

