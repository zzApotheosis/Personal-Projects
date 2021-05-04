#!/usr/bin/perl -w

# Main Class
package Main;

# Imports
use strict;
use warnings;
use FindBin;
use Cwd;
use IO::Socket::UNIX;

# Global variables
my $exec_name = "$FindBin::RealScript";
my $exec_path = "$FindBin::RealBin/$FindBin::RealScript";
my $exec_dir = "$FindBin::RealBin";
my $original_cwd = cwd();

# Main
sub main {
    # Define subroutine variables
    my $exit_code = 0;
    my $socket = "socket";
    my $msg;
    
    # Create UNIX Socket Client Object
    my $client = IO::Socket::UNIX->new(Type => SOCK_STREAM(), Peer => $socket);
    $client->autoflush(1);

    # Loop stdin and write to socket
    print(STDOUT "Begin writing to Unix socket. Enter \"\\EXIT\" to exit\n");
    while (1) {
        chomp($msg = <STDIN>);
        last if ($msg eq "\\EXIT");
        $client->send($msg);
    }

    $client->shutdown(SHUT_WR);
    $client->close();

    # End subroutine
    return $exit_code;
}

# End Main Class
1;

# Execute
exit(Main::main());

