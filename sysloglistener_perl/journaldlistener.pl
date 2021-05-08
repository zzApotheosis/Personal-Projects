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
use Sys::Syslog qw(:standard :macros);

# Global variables
my $exec_name = "$FindBin::RealScript";
my $exec_path = "$FindBin::RealBin/$FindBin::RealScript";
my $exec_dir = "$FindBin::RealBin";
my $original_cwd = cwd();
my $listener_socket = "socket";
my $identifier = $exec_name; $identifier =~ s/\.[^.]+$//;

# Declare constants;
my $MAX_BUF_SIZE = 1024;

# Main
sub main {
    # Define subroutine variables
    my $exit_code = 0;
    my $pid;
    my $client;
    my $server;
    my $conn;
    my $conn_ctr = 0;
    my $msg;
    my $status = 0;

    # Set up environment
    if (-S $listener_socket) {
        unlink($listener_socket);
    }

    # Fork and make child start Unix socket server
    $pid = fork();

    # Spin up socket listener on concurrent thread
    if (!$pid) {
        openlog($identifier, "", LOG_USER);
        $server = IO::Socket::UNIX->new(Type => SOCK_STREAM, Local => $listener_socket, Listen => 1);
        STDOUT->printflush("Listening for connections on $listener_socket\n");
        while ($conn = $server->accept()) {
            $conn_ctr++;
            $conn->recv($msg, $MAX_BUF_SIZE);
            syslog(LOG_INFO, $msg);
            $conn->shutdown(SHUT_RDWR);
            $conn->close();
        }
        return $status;
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
    STDOUT->printflush("Begin main loop. Use \"exit\" to end program.\n");
    while (1) {
        chomp($msg = <STDIN>);
        last if $msg eq "exit";
        $client = IO::Socket::UNIX->new(Type => SOCK_STREAM, Peer => $listener_socket);
        $client->send($msg);
        # $client->recv($msg, 100);
        # print(STDOUT $msg . "\n");
        $client->shutdown(SHUT_RDWR);
        $client->close();
    }

    # Clean up
    kill('SIGKILL', $pid);
    if (-S $listener_socket) {
        unlink($listener_socket);
    }

    # End subroutine
    return $exit_code;
}

# End Main Class
1;

# Execute
exit(Main::main());
