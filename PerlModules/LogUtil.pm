# LogUtil Class
package LogUtil;

# Class information
our $VERSION = "0.0.1-20210509";

# Imports
use strict;
use warnings;
use Sys::Syslog qw(:standard :macros);
use IO::Socket::UNIX;
use Scalar::Util qw(looks_like_number);
use File::Basename;
use Time::HiRes qw(usleep);

# Static fields
my $socket = undef;
my $server = undef;
my $client = undef;
my $log_file = undef;
my $identifier = undef;
my $pid = 0;

# Constants
use constant MAX_BUF_SIZE => 1024;

# Handle default signals
$SIG{INT} = \&LogUtil::sigint_handler_client;
$SIG{TERM} = \&LogUtil::sigint_handler_client;

#
# listen()
#
# This subroutine will establish a Unix Domain Socket to begin listening for
# send() calls. Properly formatted calls to send() will write log levels
# and messages to syslog and additionally a log file, if configured with
# set_log_file().
#
sub listen {
    # Define subroutine variables
    my $fh; # File handle
    my $db; # Data Buffer
    my $d; # Data
    my $c; # Connection

    # Check for existing child process
    if ($pid) {
        STDERR->printflush((caller(0))[3] . " - Already listening for messages\n");
        return;
    }

    # Prepare for listener
    unlink($socket);
    $ENV{'SYSLOG_LISTENER_SOCK'} = $socket;

    # Fork the process to handle listening and logging
    $pid = fork();
    if (!$pid) {
        $SIG{TERM} = \&LogUtil::sigint_handler_server;
        $SIG{INT} = \&LogUtil::sigint_handler_server;
        openlog($identifier, '', LOG_USER);
        $server = IO::Socket::UNIX->new(Type => SOCK_STREAM, Local => $socket, Listen => 1);
        $server->autoflush(1);
        while ($c = $server->accept()) {
            $d = "";
            while (1) {
                $c->recv($db, MAX_BUF_SIZE);
                last if length($db) == 0;
                $d .= $db;
            }
            if ($d =~ /^(\d+)\n*(.+)/) {
                next if (!looks_like_number($1));
                syslog($1, $2);
            }
            $c->shutdown(SHUT_RDWR);
            $c->close();
        }
        exit(0);
    }
    while (! -S $socket) {
        usleep(10);
    }
}

#
# set_syslogmask($m)
#
# This subroutine sets a mask for syslog calls to respect.
#
# Args:
#   $m • The new mask to apply. This argument is already expected
#        to be in the format needed to pass on to setlogmask(),
#        so callers should use Sys::Syslog qw(:macros) prior to
#        calling this subroutine.
#
sub set_syslogmask {
    # Define subroutine variables
    my $m;

    # Check for arguments
    if (!scalar(@_)) {
        return;
    }

    # Fetch arguments
    ($m) = @_;

    # Set the syslog mask
    return setlogmask($m);
}

#
# reset()
#
# This subroutine will reset all static fields back to their default
# values.
#
sub reset {
    usleep(100000); # Sleep for pending log writes
    if (defined($server)) {
        $server->shutdown(SHUT_RDWR);
        $server->close();
    }
    if ($pid) {
        kill('SIGTERM', $pid);
    }
    $socket = undef;
    $server = undef;
    $client = undef;
    $log_file = undef;
    $identifier = undef;
    $pid = 0;
    delete($ENV{'SYSLOG_LISTENER_SOCK'});
}

#
# stop()
#
# This subroutine is responsible for closing an active socket listener.
#
sub stop {
    if ($pid) {
        kill('SIGTERM', $pid);
        $pid = 0;
    }
}

#
# dump()
#
# This is a debug subroutine for development purposes only.
#
sub dump {
    STDERR->printflush("Dumping current " . (caller(0))[3] . " fields!\n");
    STDERR->printflush("socket = $socket\n") if defined($socket);
    STDERR->printflush("server = $server\n") if defined($server);
    STDERR->printflush("client = $client\n") if defined($client);
    STDERR->printflush("log_file = $log_file\n") if defined($log_file);
    STDERR->printflush("identifier = $identifier\n") if defined($identifier);
    STDERR->printflush("pid = $pid\n") if defined($pid);
}

#
# send()
#
# This subroutine is responsible for accepting log data and
# passing it onto the syslog listener child process, and/or
# a log file, if configured previously with set_log_file().
#
# Args:
#   LOG_LEVEL • The log level to use with syslog. Must
#               be a valid syslog level.
#   MESSAGE • The message to write to syslog/logfile.
#
# Note: Calls to this subroutine must use named arguments,
#       i.e. Logger::send(LOG_LEVEL => LOG_INFO, MESSAGE =>
#       "Hello, world");
#
sub send {
    # Define subroutine variables
    my %args;
    my $fh;
    my $default_log_file = "default.log";

    # Fetch arguments
    %args = (
        LOG_LEVEL => LOG_INFO,
        MESSAGE   => undef,
        @_,
    );
    chomp(%args);

    # Check if LOG_LEVEL was passed correctly
    return if (!looks_like_number($args{'LOG_LEVEL'}));

    # Check if message was provided
    return if (!defined('MESSAGE'));

    # Check if log level is valid
    if ($args{'LOG_LEVEL'} != LOG_DEBUG &&
        $args{'LOG_LEVEL'} != LOG_INFO &&
        $args{'LOG_LEVEL'} != LOG_NOTICE &&
        $args{'LOG_LEVEL'} != LOG_WARNING &&
        $args{'LOG_LEVEL'} != LOG_ERR &&
        $args{'LOG_LEVEL'} != LOG_CRIT &&
        $args{'LOG_LEVEL'} != LOG_ALERT &&
        $args{'LOG_LEVEL'} != LOG_EMERG) {
        return;
    }

    # If a log file is defined, append to it
    if (defined($log_file)) {
        if (-w dirname($log_file)) {
            open($fh, '>>', $log_file) or warn($!);
            print($fh localtime() . " • " . LogUtil::loglevel_to_bareword($args{'LOG_LEVEL'}) . " • " . $args{'MESSAGE'} . "\n");
            close($fh);
        } else {
            STDERR->printflush((caller(0))[3] . " - Unable to write to $log_file\n");
        }
    }

    # Check if child process is listening
    if ($pid) {
        $client = IO::Socket::UNIX->new(Type => SOCK_STREAM, Peer => $socket);
        if (!defined($client)) {
            STDERR->printflush((caller(0))[3] .  " - Unable to connect to $socket\n");
            return;
        }
        $client->autoflush(1);
        $client->send($args{'LOG_LEVEL'} . "\n");
        $client->send($args{'MESSAGE'});
        $client->shutdown(SHUT_RDWR);
        $client->close();
        $client = undef;
    }

    # Check if neither a server socket or log file are used
    if (!defined($log_file) && !$pid) {
        STDERR->printflush((caller(0))[3] . " - No log destinations are configured. Sending subsequent messages to $default_log_file\n");
        LogUtil::set_log_file($default_log_file);
        LogUtil::send(%args);
    }
}

#
# loglevel_to_bareword()
#
# This subroutine accepts a syslog LOG_LEVEL and returns its
# corresponding bareword.
#
# Args:
#   $ll • This is the syslog LOG_LEVEL to match to a bareword.
#
sub loglevel_to_bareword {
    # Define subroutine variables
    my $ll;

    # Check for arguments
    return if (!scalar(@_));

    # Fetch arguments
    ($ll) = @_;

    # Match LOG_LEVEL
    return "LOG_EMERG"   if ($ll == 0);
    return "LOG_ALERT"   if ($ll == 1);
    return "LOG_CRIT"    if ($ll == 2);
    return "LOG_ERR"     if ($ll == 3);
    return "LOG_WARNING" if ($ll == 4);
    return "LOG_NOTICE"  if ($ll == 5);
    return "LOG_INFO"    if ($ll == 6);
    return "LOG_DEBUG"   if ($ll == 7);
    return undef;
}

#
# sigint_handler_server()
#
# This subroutine handles the SIGTERM signal for the child process.
#
sub sigint_handler_server {
    $server->shutdown(SHUT_RDWR) if (defined($server));
    $server->close() if (defined($server));
    exit(0);
}

#
# sigint_handler_client()
#
# This subroutine handles the SIGTERM signal for the parent process.
#
sub sigint_handler_client {
    $client->shutdown(SHUT_RDWR) if (defined($client));
    $client->close() if (defined($client));
    kill('SIGTERM', $pid) if ($pid);
    exit(0);
}

#
# Setters and getters
#
sub set_socket {$socket = defined($_[0]) ? $_[0] : $socket;}
sub get_socket {return $socket;}
sub set_server {$server = defined($_[0]) ? $_[0] : $server;}
sub get_server {return $server;}
sub set_client {$client = defined($_[0]) ? $_[0] : $client;}
sub get_client {return $client;}
sub set_log_file {$log_file = defined($_[0]) ? $_[0] : $log_file;}
sub get_log_file {return $log_file;}
sub set_identifier {$identifier = defined($_[0]) ? $_[0] : $identifier;}
sub get_identifier {return $identifier;}

# End LogUtil Class
1;
