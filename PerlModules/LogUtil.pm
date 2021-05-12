# LogUtil Class
package LogUtil;

# Class information
our $VERSION = "0.2.0-20210510";

# Constants
use constant MAX_BUF_SIZE => 1024;
use constant UNDEFINED_IDENTIFIER => 'undefined_identifier';
use constant DEFAULT_LOG_FILE     => 'default.log';
use constant LOG_FILE_ERR   => 0b00001;
use constant LOG_LEVEL_ERR  => 0b00010;
use constant FIFO_ERR       => 0b00100;
use constant SOCKET_ERR     => 0b01000;
use constant IDENTIFIER_ERR => 0b10000;

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
my $default_log_file = LogUtil::DEFAULT_LOG_FILE;
my $default_log_level = LOG_INFO;
my $default_fifo = undef;
my $default_fifo_mode = 0700;
my $default_socket = undef;
my $default_identifier = undef;

#
# set_default_log_file()
#
# A static subroutine to set the class default log file.
#
sub set_default_log_file {
    return if (!defined($_[0]));
    $default_log_file = $_[0];
}

#
# set_default_log_level()
#
# A statuc subroutine to set the class default log level.
#
sub set_default_log_level {
    return if (!defined($_[0]));
    $default_log_level = $_[0];
}

#
# set_default_fifo()
#
# A static subroutine to set the class default fifo.
#
sub set_default_fifo {
    return if (!defined($_[0]));
    $default_fifo = $_[0];
}

#
# set_default_fifo()
#
# A static subroutine to set the class default fifo.
#
sub set_default_fifo_mode {
    return if (!defined($_[0]));
    $default_fifo_mode = $_[0];
}

#
# set_default_socket()
#
# A static subroutine to set the class default socket.
#
sub set_default_socket {
    return if (!defined($_[0]));
    $default_socket = $_[0];
}

#
# set_default_identifier()
#
# A static subroutine to set the class default identifier.
#
sub set_default_identifier {
    return if (!defined($_[0]));
    $default_identifier = $_[0];
}

#
# get_default_log_file()
#
# A static subroutine to get the class default log file.
#
sub get_default_log_file {
    return $default_log_file;
}

#
# get_default_log_level()
#
# A static subroutine to get the class default log level.
#
sub get_default_log_level {
    return $default_log_level;
}

#
# get_default_fifo()
#
# A static subroutine to get the class default fifo.
#
sub get_default_fifo {
    return $default_fifo;
}

#
# get_default_fifo()
#
# A static subroutine to get the class default fifo.
#
sub get_default_fifo_mode {
    return $default_fifo_mode;
}

#
# get_default_socket()
#
# A static subroutine to get the class default socket.
#
sub get_default_socket {
    return $default_socket;
}

#
# get_default_identifier()
#
# A static subroutine to get the class default identifier.
#
sub get_default_identifier {
    return $default_identifier;
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
    return "LOG_EMERG"   if ($ll == LOG_EMERG);
    return "LOG_ALERT"   if ($ll == LOG_ALERT);
    return "LOG_CRIT"    if ($ll == LOG_CRIT);
    return "LOG_ERR"     if ($ll == LOG_ERR);
    return "LOG_WARNING" if ($ll == LOG_WARNING);
    return "LOG_NOTICE"  if ($ll == LOG_NOTICE);
    return "LOG_INFO"    if ($ll == LOG_INFO);
    return "LOG_DEBUG"   if ($ll == LOG_DEBUG);
    return undef;
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
# check_log_level()
#
# A subroutine to check whether or not a given log level is valid.
#
sub check_log_level {
    return 1 if (!defined($_[0]));
    return 1 if (!looks_like_number($_[0]));
    return 0 if ($_[0] >= LOG_EMERG && $_[0] <= LOG_DEBUG);
}

#
# sigterm_handler()
#
# This subroutine handles the SIGTERM signal for the child process.
#
sub sigterm_handler {
    my %args;
    %args = @_;
    $args{server_ptr} = defined($args{server_ptr}) ? $args{server_ptr} : undef;
    $args{fifo} = defined($args{fifo}) ? $args{fifo} : undef;
    $args{socket} = defined($args{socket}) ? $args{socket} : undef;
    if (defined($args{server_ptr})) {
        ${$args{server_ptr}}->shutdown(SHUT_RDWR);
        ${$args{server_ptr}}->close();
    }
    unlink($args{fifo}) if (defined($args{fifo}) && -p $args{fifo});
    unlink($args{socket}) if (defined($args{socket}) && -S $args{socket});
    exit(EXIT_SUCCESS);
}

#
# new()
#
# This is the constructor for this class. IT accepts named arguments only, or no
# arguments at all.
#
sub new {
    # Define subroutine variables
    my %args;
    my $class = shift(@_);
    my $self;

    # Fetch args
    %args = @_;
    $args{socket} = defined($args{socket}) ? $args{socket} : LogUtil::get_default_socket();
    $args{fifo} = defined($args{fifo}) ? $args{fifo} : LogUtil::get_default_fifo();
    $args{fifo_mode} = defined($args{fifo_mode}) ? $args{fifo_mode} : LogUtil::get_default_fifo_mode();
    $args{log_file} = defined($args{log_file}) ? $args{log_file} : LogUtil::get_default_log_file();
    $args{log_level} = defined($args{log_level}) ? $args{log_level} : LogUtil::get_default_log_level();
    $args{identifier} = defined($args{identifier}) ? $args{identifier} : LogUtil::get_default_identifier();

    # Initialize object
    $self = {
        _socket      => $args{socket},
        _fifo        => $args{fifo},
        _fifo_mode   => $args{fifo_mode},
        _log_file    => $args{log_file},
        _log_level   => $args{log_level},
        _identifier  => $args{identifier},
        _socket_cpid => 0,
        _fifo_cpid   => 0
    };

    # Instantiate object
    bless($self, $class);

    # Return new object
    return $self;
}

#
# listen()
#
# This subroutine will establish a Unix Domain Socket to begin listening for
# send() calls. Properly formatted calls to send() will write log levels
# and messages to syslog and additionally a log file, if configured with
# set('_log_file').
#
sub listen {
    # Define subroutine variables and constants
    my $self = shift(@_);
    my %args;
    my $server;
    my $pid;
    my $fh; # File handle
    my $db; # Data Buffer
    my $d; # Data
    my $c; # Connection
    my $ll; # Log level
    my $msg; # Message
    my $status = EXIT_SUCCESS;

    # Fetch arguments
    %args = @_;
    # Default values
    $args{socket} = defined($args{socket}) ? $args{socket} : defined($self->get_socket()) ? $self->get_socket() : LogUtil::get_default_socket();
    $args{fifo} = defined($args{fifo}) ? $args{fifo} : defined($self->get_fifo()) ? $self->get_fifo() : LogUtil::get_default_fifo();
    $args{fifo_mode} = defined($args{fifo_mode}) ? $args{fifo_mode} : defined($self->get_fifo_mode()) ? $self->get_default_fifo_mode() : LogUtil::get_default_fifo_mode();
    $args{identifier} = defined($args{identifier}) ? $args{identifier} : defined($self->get_identifier()) ? $self->get_identifier() : LogUtil::get_default_identifier();

    # First, establish a fifo to listen on if defined
    if (defined($args{fifo})) {
        # Disallow null characters in fifo name
        if (!grep(/\0/, $args{fifo})) {
            # Check for identifier
            if (defined($args{identifier})) {
                # Check for existing fifo
                if ($self->get_fifo_cpid()) {
                    kill('SIGINT', $self->get_fifo_cpid());
                    $self->set_fifo_cpid(0);
                }

                # Prepare to establish fifo
                unlink($args{fifo}) if -p $args{fifo};
                $ENV{'LOGUTIL_FIFO'} = $args{fifo};

                # Fork so child may serve connections
                $pid = fork();
                if (!$pid) {
                    $SIG{TERM} = sub { LogUtil::sigterm_handler(fifo => $args{fifo}) };
                    $SIG{INT} = sub { LogUtil::sigterm_handler(fifo => $args{fifo}) };
                    openlog($args{identifier}, '', LOG_USER);
                    mkfifo($args{fifo}, $args{fifo_mode});
                    open($fh, '+<', $args{fifo}) or die($!);
                    while (1) {
                        $ll = <$fh>;
                        next if (!defined($ll));
                        chomp($ll);
                        next if (!looks_like_number($ll));
                        $msg = <$fh>;
                        next if (!defined($msg));
                        chomp($msg);
                        syslog($ll, $msg);
                    }
                    close($fh);
                    exit(EXIT_SUCCESS);
                }
                $self->set_fifo_cpid($pid);

                # In parent process, wait for fifo to be established
                while (! -p $args{fifo}) {
                    usleep(10);
                }
            } else {
                STDERR->printflush((caller(0))[3] . " - Error trying to establish fifo listener: Cannot write to fifo/syslog without an identifier\n");
                $status |= FIFO_ERR;
                $status |= IDENTIFIER_ERR;
            }
        } else {
            STDERR->printflush((caller(0))[3] . " - Cannot use null characters in fifo name. Perl isn't cool like C\n");
            $status |= FIFO_ERR;
        }
    }

    # Next, establish a socket to listen on if defined
    if (defined($args{socket})) {
        # Disallow null characters in socket name
        if (!grep(/\0/, $args{socket})) {
            # Check for identifier
            if (defined($args{identifier})) {
                # Check for existing socket
                if ($self->get_socket_cpid()) {
                    kill('SIGINT', $self->get_socket_cpid());
                    $self->set_socket_cpid(0);
                }

                # Prepare to establish socket
                unlink($args{socket}) if (-S $args{socket});
                $ENV{LOGUTIL_SOCK} = $args{socket};

                # Fork so child may serve connections
                $pid = fork();
                if (!$pid) {
                    openlog($args{identifier}, '', LOG_USER);
                    $server = IO::Socket::UNIX->new(Type => SOCK_STREAM, Local => $self->get_socket(), Listen => 1);
                    $SIG{TERM} = sub { LogUtil::sigterm_handler(server_ptr => \$server) };
                    $SIG{INT} = sub { LogUtil::sigterm_handler(server_ptr => \$server) };
                    if (!defined($server)) {
                        STDERR->printflush((caller(0))[3] . " - Error starting socket listener\n");
                        exit(EXIT_FAILURE);
                    }
                    $server->autoflush(1);
                    while ($c = $server->accept()) {
                        $d = '';
                        while (1) {
                            $c->recv($db, MAX_BUF_SIZE);
                            last if length($db) == 0;
                            $d .= $db;
                        }
                        if ($d =~ /^(.+)\0(.+)\0$/) {
                            if (looks_like_number($1)) {
                                syslog($1, $2);
                            }
                        }
                        $c->shutdown(SHUT_RDWR);
                        $c->close();
                    }
                    $c->shutdown(SHUT_RDWR);
                    $c->close();
                    exit(EXIT_SUCCESS);
                }
                $self->set_socket_cpid($pid);

                # In parent process, wait for socket to be established
                while (! -S $args{socket}) {
                    usleep(10);
                }
            } else {
                STDERR->printflush((caller(0))[3] . " - Error trying to establish socket listener: Cannot write to socket/syslog without an identifier\n");
                $status |= SOCKET_ERR;
                $status |= IDENTIFIER_ERR;
            }
        } else {
            STDERR->printflush((caller(0))[3] . " - Cannot use null characters in socket name. Perl isn't cool like C\n");
            $status |= SOCKET_ERR;
        }
    }

    # Return listen() status
    return $status;
}

#
# send()
#
# This subroutine is responsible for accepting log data and
# passing it onto the syslog listener child process, and/or
# a log file, if configured previously with set_log_file().
#
# Note: Calls to this subroutine must use named arguments,
#       i.e. send(log_info => LOG_INFO, message =>
#       "Hello, world");
#
sub send {
    # Define subroutine variables and constants
    my $status = EXIT_SUCCESS;
    my $self = shift(@_);
    my %args;
    my $client;
    my $fh;

    # Fetch arguments
    %args = @_;
    # Default values
    $args{message} = defined($args{message}) ? $args{message} : undef;
    $args{log_level} = defined($args{log_level}) ? $args{log_level} : defined($self->get_log_level()) ? $self->get_default_log_level() : LogUtil::get_default_log_level();
    $args{socket} = defined($args{socket}) ? $args{socket} : defined($self->get_socket()) ? $self->get_socket() : LogUtil::get_default_socket();
    $args{fifo} = defined($args{fifo}) ? $args{fifo} : defined($self->get_fifo()) ? $self->get_fifo() : LogUtil::get_default_fifo();
    $args{log_file} = defined($args{log_file}) ? $args{log_file} : defined($self->get_log_file()) ? $self->get_log_file() : LogUtil::get_default_log_file();
    $args{identifier} = defined($args{identifier}) ? $args{identifier} : defined($self->get_identifier()) ? $self->get_identifier() : LogUtil::get_default_identifier();
    foreach my $key (keys(%args)) {
        if (defined($args{$key})) {
            chomp($args{$key});
        }
    }

    # Check if log_level was passed correctly
    return if (!defined($args{log_level}) || !looks_like_number($args{log_level}));

    # Check if message was provided
    return if (!defined($args{message}));

    # Check if log level is valid
    return if ($args{log_level} < LOG_EMERG || $args{log_level} > LOG_DEBUG);

    # Check if nothing is configured for logging
    if (!defined($args{log_file}) && !defined($args{fifo}) && !defined($args{socket})) {
        STDERR->printflush((caller(0))[3] . " - No log destinations are configured. Sending subsequent messages to " . DEFAULT_LOG_FILE . "\n");
        LogUtil::set_default_log_file(DEFAULT_LOG_FILE);
        return LogUtil::send(message => $args{message});
    }

    # If a log file is defined, append to it
    if (defined($args{log_file})) {
        if (-w dirname($args{log_file})) {
            if (open($fh, '>>', $args{log_file})) {
                print($fh localtime() . " • " . (defined($args{identifier}) ? $args{identifier} : UNDEFINED_IDENTIFIER) . " • " . LogUtil::loglevel_to_bareword($args{log_level}) . " • " . $args{message} . "\n");
                close($fh);
            } else {
                warn($!);
                $status |= LOG_FILE_ERR;
            }
        } else {
            STDERR->printflush((caller(0))[3] . " - Unable to write to " . $args{log_file} . "\n");
            $status |= LOG_FILE_ERR;
        }
    }

    # Try to write to fifo
    if (defined($args{fifo})) {
        if (-p $args{fifo}) {
            if (open($fh, '>', $args{fifo})) {
                $fh->printflush('');
                $fh->printflush($args{log_level} . "\n");
                $fh->printflush($args{message} . "\n");
                close($fh);
            } else {
                STDERR->printflush((caller(0))[3] . " - Unable to open " . $args{fifo} . "\n");
                $status |= FIFO_ERR;
            }
        } else {
            STDERR->printflush((caller(0))[3] . " - Fifo " . $args{fifo} . " does not exist\n");
            $status |= FIFO_ERR;
        }
    }

    # Try to connect to socket
    if (defined($args{socket})) {
        if (defined($client = IO::Socket::UNIX->new(Type => SOCK_STREAM, Peer => $args{socket}))) {
            $client->autoflush(1);
            $client->send($args{log_level} . "\0");
            $client->send($args{message} . "\0");
            $client->shutdown(SHUT_RDWR);
            $client->close();
            $client = undef;
        } else {
            STDERR->printflush((caller(0))[3] .  " - Unable to connect to " . $args{socket} . "\n");
            $status |= SOCKET_ERR;
        }
    }

    # Return send() status
    return $status;
}

#
# close()
#
# A subroutine to handle closing currently tracked sockets and/or fifos. It is
# very important to call this subroutine some time after calling listen()
# or there will be orphaned processes.
#
sub close {
    my $self = shift(@_);
    usleep(100000); # 1/10 second sleep to allow logs to finish writing
    kill('SIGTERM', $self->get_fifo_cpid()) if (defined($self->get_fifo_cpid()) && $self->get_fifo_cpid());
    kill('SIGTERM', $self->get_socket_cpid()) if (defined($self->get_socket_cpid()) && $self->get_socket_cpid());
}

#
# Setters and getters
#
sub set_socket {
    return if (!defined($_[0]) && !defined($_[1]));
    $_[0]->{_socket} = $_[1];
}

sub set_fifo {
    return if (!defined($_[0]) && !defined($_[1]));
    $_[0]->{_fifo} = $_[1];
}

sub set_fifo_mode {
    return if (!defined($_[0]) && !defined($_[1]));
    $_[0]->{_fifo_mode} = $_[1];
}

sub set_log_file {
    return if (!defined($_[0]) && !defined($_[1]));
    $_[0]->{_log_file} = $_[1];
}

sub set_log_level {
    return if (!defined($_[0]) && !defined($_[1]));
    $_[0]->{_log_level} = $_[1];
}

sub set_identifier {
    return if (!defined($_[0]) && !defined($_[1]));
    $_[0]->{_identifier} = $_[1];
}

sub set_socket_cpid {
    return if (!defined($_[0]) && !defined($_[1]));
    $_[0]->{_socket_cpid} = $_[1];
}

sub set_fifo_cpid {
    return if (!defined($_[0]) && !defined($_[1]));
    $_[0]->{_fifo_cpid} = $_[1];
}

sub get_socket {
    return $_[0]->{_socket} if defined($_[0]);
}

sub get_fifo {
    return $_[0]->{_fifo} if defined($_[0]);
}

sub get_fifo_mode {
    return $_[0]->{_fifo_mode} if defined($_[0]);
}

sub get_log_file {
    return $_[0]->{_log_file} if defined($_[0]);
}

sub get_log_level {
    return $_[0]->{_log_level} if defined($_[0]);
}

sub get_identifier {
    return $_[0]->{_identifier} if defined($_[0]);
}

sub get_socket_cpid {
    return $_[0]->{_socket_cpid} if defined($_[0]);
}

sub get_fifo_cpid {
    return $_[0]->{_fifo_cpid} if defined($_[0]);
}

# End LogUtil Class
1;
