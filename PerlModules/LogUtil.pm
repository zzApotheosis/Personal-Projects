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
use POSIX;

# Static fields
my $default_socket = undef;
my $default_fifo = undef;
my $default_log_file = undef;
my $server = undef;
my $identifier = undef;
my $pid = 0;

# Constants
use constant MAX_BUF_SIZE => 1024;

#
# listen()
#
# This subroutine will establish a Unix Domain Socket to begin listening for
# send() calls. Properly formatted calls to send() will write log levels
# and messages to syslog and additionally a log file, if configured with
# set_log_file().
#
sub listen {
    # Define subroutine variables and constants
    my %args;
    my $fh; # File handle
    my $db; # Data Buffer
    my $d; # Data
    my $c; # Connection
    my $status = EXIT_SUCCESS;
    use constant SOCKET_ERR => 0b1;

    # Fetch arguments
    %args = @_;
    $args{'SOCKET'}

    # Check for existing child process
    if ($pid) {
        STDERR->printflush((caller(0))[3] . " - Already listening for messages\n");
        return;
    }

    # Prepare for listener
    unlink($socket);
    $ENV{'LOGUTIL_SOCK'} = $socket;

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
#   SOCKET • The socket with which to attempt to establish a connection.
#   LOG_FILE • The path of the log file to write to.
#
# Note: Calls to this subroutine must use named arguments,
#       i.e. Logger::send(LOG_LEVEL => LOG_INFO, MESSAGE =>
#       "Hello, world");
#
sub send {
    # Define subroutine variables and constants
    my %args;
    my $client;
    my $fh;
    my $default_log_file = "default.log";
    my $status = EXIT_SUCCESS;
    use constant SOCKET_ERROR => 0b01;
    use constant LOG_FILE_ERROR => 0b10;

    # Fetch arguments
    %args = @_;
    # Default values
    $args{'LOG_LEVEL'} = defined($args{'LOG_VALUE'}) ? $args{'LOG_VALUE'} : LOG_INFO;
    $args{'MESSAGE'} = defined($args{'MESSAGE'}) ? $args{'MESSAGE'} : undef;
    $args{'SOCKET'} = defined($args{'SOCKET'}) ? $args{'SOCKET'} : $socket;
    $args{'LOG_FILE'} = defined($args{'LOG_FILE'}) ? $args{'LOG_FILE'} : $log_file;
    foreach my $key (keys(%args)) {
        if (defined($args{$key})) {
            chomp($args{$key});
        }
    }

    # Check if LOG_LEVEL was passed correctly
    return if (!defined($args{'LOG_LEVEL'}) || !looks_like_number($args{'LOG_LEVEL'}));

    # Check if message was provided
    return if (!defined($args{'MESSAGE'}));

    # Check if log level is valid
    return if ($args{'LOG_LEVEL'} != LOG_DEBUG &&
        $args{'LOG_LEVEL'} != LOG_INFO &&
        $args{'LOG_LEVEL'} != LOG_NOTICE &&
        $args{'LOG_LEVEL'} != LOG_WARNING &&
        $args{'LOG_LEVEL'} != LOG_ERR &&
        $args{'LOG_LEVEL'} != LOG_CRIT &&
        $args{'LOG_LEVEL'} != LOG_ALERT &&
        $args{'LOG_LEVEL'} != LOG_EMERG);

    # If a log file is defined, append to it
    if (defined($args{'LOG_FILE'})) {
        if (-w dirname($args{'LOG_FILE'})) {
            if (open($fh, '>>', $args{'LOG_FILE'})) {
                print($fh localtime() . " • " . LogUtil::loglevel_to_bareword($args{'LOG_LEVEL'}) . " • " . $args{'MESSAGE'} . "\n");
                close($fh);
            } else {
                warn($!);
                $status |= LOG_FILE_ERROR;
            }
        } else {
            STDERR->printflush((caller(0))[3] . " - Unable to write to " . $args{'LOG_FILE'} . "\n");
            $status |= LOG_FILE_ERROR;
        }
    }

    # Try to connect to socket
    if (defined($args{'SOCKET'})) {
        if (defined($client = IO::Socket::UNIX->new(Type => SOCK_STREAM, Peer => $args{'SOCKET'}))) {
            $client->autoflush(1);
            $client->send($args{'LOG_LEVEL'} . "\n");
            $client->send($args{'MESSAGE'});
            $client->shutdown(SHUT_RDWR);
            $client->close();
            $client = undef;
        } else {
            STDERR->printflush((caller(0))[3] .  " - Unable to connect to " . $args{'SOCKET'} . "\n");
            $status |= SOCKET_ERROR;
        }
    }

    # Check if neither a server socket or log file are used
    if (!defined($args{'LOG_FILE'}) && (!defined($args{'SOCKET'}))) {
        STDERR->printflush((caller(0))[3] . " - No log destinations are configured. Sending subsequent messages to $default_log_file\n");
        LogUtil::set_log_file($default_log_file);
        return LogUtil::send(LOG_LEVEL => $args{'LOG_LEVEL'}, MESSAGE => $args{'MESSAGE'});
    }

    # Return send() status
    return $status;
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
    exit(EXIT_SUCCESS);
}

#
# Setters and getters
#
sub set_default_socket {$default_socket = defined($_[0]) ? $_[0] : $default_socket;}
sub get_default_socket {return $default_socket;}
sub set_default_fifo {$default_fifo = defined($_[0]) ? $_[0] : $default_fifo;}
sub get_default_fifo {return $default_fifo;}
sub set_default_log_file {$default_log_file = defined($_[0]) ? $_[0] : $default_log_file;}
sub get_default_log_file {return $default_log_file;}
sub set_server {$server = defined($_[0]) ? $_[0] : $server;}
sub get_server {return $server;}
sub set_identifier {$identifier = defined($_[0]) ? $_[0] : $identifier;}
sub get_identifier {return $identifier;}

# End LogUtil Class
1;
