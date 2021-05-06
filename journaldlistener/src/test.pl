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
    my $pid;
    my $client;
    my $msg;
    my $status;

    # Fork and make child start Unix socket server
    $pid = fork();

    # Branch behavior
    if (!$pid) {
        print(STDOUT "Starting Unix socket listener from Perl\n");
        exec("$exec_dir/a.out");
    }

    # Allow time to establish the Unix socket listener
    sleep(1);

    # Loop
    # Client::init();
    while (1) {
        chomp($msg = <STDIN>);
        last if $msg eq "EXIT";
        Client::init();
        Client::send_message($msg);
        print(STDOUT Client::read_message(length($msg)) . "\n");
        Client::stop();
    }
    # Client::stop();
    kill('SIGINT', $pid);

    # End subroutine
    return $exit_code;
}

# End Main Class
1;

# Unix Socket Client Class
package Client;

# Imports
use strict;
use warnings;
use IO::Socket::UNIX;

# Static variables
my $client;

# init subroutine
sub init {
    # Create Unix Socket Client
    $client = IO::Socket::UNIX->new(Type => SOCK_STREAM(), Peer => "socket");
    if (!defined($client)) {
        print(STDERR (caller(0))[3] . " - Failed to create client\n");
        return;
    }
    $client->autoflush(1);
}

# send_message subroutine
sub send_message {
    # Define subroutine variables
    my @messages;

    # Check for arguments
    if (!scalar(@_)) {
        return;
    }

    # Fetch arguments
    @messages = @_;

    # Send messages
    foreach (@messages) {
        $client->send($_);
    }
}

# read_message subroutine
sub read_message {
    # Define subroutine variables
    my $message = "";
    my $s;

    # Check for arguments
    if (!scalar(@_)) {
        return;
    }

    # Fetch arguments
    ($s) = @_;

    # Get messages from socket connection
    $client->recv($message, $s);

    # Return message
    return $message;
}

# close subroutine
sub stop {
    $client->shutdown(SHUT_RDWR);
    $client->close();
}

# End Unix Socket Client Class
1;

# Execute
exit(Main::main());
