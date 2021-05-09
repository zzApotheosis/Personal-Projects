# Logger Class
package Logger;

# Imports
use strict;
use warnings;
use Sys::Syslog qw(:standard :macros);
use POSIX qw(mkfifo);
use Scalar::Util qw(looks_like_number);
use File::Basename;
use Time::HiRes qw(usleep);

# Static fields
my $fifo = undef;
my $fifo_mode = 0700;
my $log_file = undef;
my $identifier = undef;
my $pid = 0;

#
# listen()
#
# This subroutine will establish a named pipe (FIFO) to begin listening for
# send() calls. Properly formatted calls to send() will write log levels
# and messages to syslog and additionally a log file, if configured with
# set_log_file().
#
sub listen {
    # Define subroutine variables
    my $fh; # File handle
    my $ll; # Log level
    my $msg; # Message

    # Check for existing child process
    if ($pid) {
        Logger::send(LOG_LEVEL => LOG_NOTICE, MESSAGE => "Already listening for messages");
        return;
    }

    # Set default logmask
    Logger::set_syslogmask(LOG_UPTO(LOG_INFO));

    # Make fifo
    unlink($fifo);
    mkfifo($fifo, $fifo_mode);

    # Fork the process to handle listening and logging
    $pid = fork();

    # The child process will now listen on the named pipe (fifo)
    if (!$pid) {
        openlog($identifier, 'ndelay,nofatal,pid', LOG_USER);
        open($fh, '+<', $fifo) or die($!);
        while (1) {
            chomp($ll = <$fh>);
            next if (!looks_like_number($ll));
            chomp($msg = <$fh>);
            syslog($ll, $msg);
        }
        exit(0);
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
    usleep(10); # Sleep for pending log writes
    if ($pid) {
        kill('SIGTERM', $pid);
    }
    unlink($fifo);
    $fifo = undef;
    $fifo_mode = 0700;
    $log_file = undef;
    $identifier = undef;
    $pid = 0;
}

#
# dump()
#
# This is a debug subroutine for development purposes only.
#
sub dump {
    STDERR->printflush("Dumping current " . (caller(0))[3] . " fields!\n");
    STDERR->printflush("fifo = $fifo\n");
    STDERR->printflush("fifo_mode = $fifo_mode\n");
    STDERR->printflush("log_file = $log_file\n");
    STDERR->printflush("identifier = $identifier\n");
    STDERR->printflush("pid = $pid\n");
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

    # Fetch arguments
    %args = (
        LOG_LEVEL => LOG_INFO,
        MESSAGE   => undef,
        @_,
    );

    # Check if message was provided
    if (!defined($args{'MESSAGE'})) {
        return;
    }

    # If a log file is defined, append to it
    if (defined($log_file)) {
        if (-w dirname($log_file)) {
            open($fh, '>>', $log_file) or warn($!);
            print($fh localtime() . " • " . $args{'MESSAGE'} . "\n");
            close($fh);
        } else {
            STDERR->printflush((caller(0))[3] . " - Unable to write to $log_file\n");
        }
    }

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

    # Check if child process is listening on fifo
    if ($pid) {
        open($fh, '>', $fifo) or die($!);
        print($fh $args{'LOG_LEVEL'} . "\n");
        print($fh $args{'MESSAGE'} . "\n");
        close($fh);
    }
}

# 
# Setters and getters
#
sub set_fifo {$fifo = defined($_[0]) ? $_[0] : $fifo;}
sub get_fifo {return $fifo;}
sub set_fifo_mode {$fifo_mode = defined($_[0]) ? $_[0] : $fifo_mode;}
sub get_fifo_mode {return $fifo_mode;}
sub set_identifier {$identifier = defined($_[0]) ? $_[0] : $identifier;}
sub get_identifier {return $identifier;}
sub set_log_file {$log_file = defined($_[0]) ? $_[0] : $log_file;}
sub get_log_file {return $log_file;}

# End Logger Class
1;

