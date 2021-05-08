#!/usr/bin/perl -w

# Main Class
package Main;

# Imports
use strict;
use warnings;
use FindBin;
use Cwd;

# Class fields
my $exec_name = "$FindBin::RealScript";
my $exec_path = "$FindBin::RealBin/$FindBin::RealScript";
my $exec_dir = "$FindBin::RealBin";
my $original_cwd = cwd();

# Main subroutine
sub main {
    # Define subroutine variables
    my $exit_code = 0;
    
    # DO TEST
    Logger::test();
    
    # End main subroutine
    return $exit_code;
}

# End Main Class
1;

# Logger Class
package Logger;

# Imports
use strict;
use warnings;
use IO::Socket::UNIX;

# Class fields
sub test {
    my $client = IO::Socket::UNIX->new(Type => SOCK_STREAM, Peer => "../sysloglistener/S.sysloglistener");
    my $msg;
    return if !defined($client);
    $client->autoflush(1);
    STDOUT->printflush("Connected to Unix socket. Use \"exit\" to end program.\n");
    while (1) {
        chomp($msg = <STDIN>);
        last if $msg eq 'exit';
        $client->write($msg);
    }
    $client->shutdown(SHUT_RDWR);
    $client->close();
}

# End Logger Class
1;

# Execute
exit(Main::main());

