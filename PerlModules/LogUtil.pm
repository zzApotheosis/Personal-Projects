# LogUtil Class
package LogUtil;

# Class information
our $VERSION = "0.1.0-20210509";

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
my $default_fifo_mode = 0700;
my $default_log_file = "default.log";

# Constants
use constant MAX_BUF_SIZE => 1024;
use constant UNIX_SOCKET_TYPE => 'socket';
use constant FIFO_TYPE => 'fifo';
use constant SOCKET_ERR     => 0b001;
use constant FIFO_ERR       => 0b010;
use constant IDENTIFIER_ERR => 0b100;

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
# get_default_socket()
#
# A static subroutine to get the class default socket.
#
sub get_default_socket {
    return $default_socket;
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
# get_default_fifo()
#
# A static subroutine to get the class default fifo.
#
sub get_default_fifo {
    return $default_fifo;
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
# get_default_fifo()
#
# A static subroutine to get the class default fifo.
#
sub get_default_fifo_mode {
    return $default_fifo_mode;
}

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
# get_default_log_file()
#
# A static subroutine to get the class default log file.
#
sub get_default_log_file {
    return $default_log_file;
}

#
# sigint_handler_server()
#
# This subroutine handles the SIGTERM signal for the child process.
#
sub sigint_handler {
    # $server->shutdown(SHUT_RDWR) if (defined($server));
    # $server->close() if (defined($server));
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
    $args{'socket'} = defined($args{'socket'}) ? $args{'socket'} : LogUtil::get_default_socket();
    $args{'fifo'} = defined($args{'fifo'}) ? $args{'fifo'} : LogUtil::get_default_fifo();
    $args{'fifo_mode'} = defined($args{'fifo_mode'}) ? $args{'fifo_mode'} : LogUtil::get_default_fifo_mode();
    $args{'log_file'} = defined($args{'log_file'}) ? $args{'log_file'} : LogUtil::get_default_log_file();
    $args{'server'} = undef;
    $args{'identifier'} = defined($args{'identifier'}) ? $args{'identifier'} : undef;

    # Initialize object
    $self = {
        _socket      => $args{'socket'},
        _fifo        => $args{'fifo'},
        _fifo_mode   => $args{'fifo_mode'},
        _log_file    => $args{'log_file'},
        _server      => $args{'server'},
        _identifier  => $args{'identifier'},
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
    my $fh; # File handle
    my $db; # Data Buffer
    my $d; # Data
    my $c; # Connection
    my $ll; # Log level
    my $msg; # Message
    my $status = EXIT_SUCCESS;

    # Fetch arguments
    %args = @_;
    $args{'socket'} = defined($args{'socket'}) ? $args{'socket'} : defined($self->get_socket()) ? $self->get_socket() : LogUtil::get_default_socket();
    $args{'fifo'} = defined($args{'fifo'}) ? $args{'fifo'} : defined($self->get_fifo()) ? $self->get_fifo() : LogUtil::get_default_fifo();
    $args{'fifo_mode'} = defined($args{'fifo_mode'}) ? $args{'fifo_mode'} : defined($self->get_fifo_mode()) ? $self->get_default_fifo_mode() : LogUtil::get_default_fifo_mode();
    $args{'identifier'} = defined($args{'identifier'}) ? $args{'identifier'} : defined($self->get_identifier()) ? $self->get_identifier() : undef;

    # First, establish a socket to listen on if defined
    STDOUT->printflush("TEST\n");
    if (defined($args{'socket'})) {
        # Check for identifier
        if (defined($args{'identifier'})) {
            # Check for existing socket
            if ($self->get_socket_cpid()) {
                kill('SIGINT', $self->get_socket_cpid());
                $self->set_socket_cpid(0);
            }

            # Prepare to establish socket
            unlink($self->get_socket());
            $ENV{'LOGUTIL_SOCK'} = $args{'socket'};

            # Fork so child may serve connections
            $self->set_socket_cpid(fork());
            if (!$self->get_socket_cpid()) {
                $SIG{INT} = \&LogUtil::sigint_handler;
                $SIG{TERM} = \&LogUtil::sigint_handler;
                openlog($self->get_identifier(), '', LOG_USER);
                $server = IO::Socket::UNIX->new(Type => SOCK_STREAM, Local => $self->get_socket(), Listen => 1);
                $server->autoflush(1);
                while ($c = $server->accept()) {
                    $d = "";
                    while (1) {
                        $c->recv($db, MAX_BUF_SIZE);
                        last if length($db) == 0;
                        $d .= $db;
                    }
                    if ($d =~ /^(\d+)\n*(.+)/) {
                        if (looks_like_number($1)) {
                            syslog($1, $2);
                        }
                    }
                    $c->shutdown(SHUT_RDWR);
                    $c->close();
                }
                exit(EXIT_SUCCESS);
            }

            # In parent process, wait for socket to be established
            while (! -S $self->get_socket()) {
                usleep(10);
            }
        } else {
            STDERR->printflush((caller(0))[3] . " - Unknown identifier. Cannot establish socket listener\n");
            $status |= SOCKET_ERR;
        }
    }

    # Next, establish a fifo to listen on if defined
    if (defined($args{'fifo'})) {
        # Check for identifier
        if (defined($args{'identifier'})) {
            # Check for existing fifo
            if ($self->get_fifo_cpid()) {
                kill('SIGINT', $self->get_fifo_cpid());
                $self->set_fifo_cpid(0);
            }

            # Prepare to establish fifo
            unlink($self->get_fifo());
            $ENV{'LOGUTIL_FIFO'} = $self->get_fifo();

            # Fork so child may serve connections
            $self->set_fifo_cpid(fork());
            if (!$self->get_fifo_cpid()) {
                $SIG{INT} = \&LogUtil::sigint_handler;
                $SIG{TERM} = \&LogUtil::sigint_handler;
                openlog($self->get_identifier(), '', LOG_USER);
                mkfifo($self->get_fifo(), defined($args{'fifo_mode'}) ? $args{'fifo_mode'} : $default_fifo_mode);
                open($fh, '+<', $self->get_fifo()) or die($!);
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

            # In parent process, wait for fifo to be established
            while (! -S $self->get_fifo()) {
                usleep(10);
            }
        } else {
            STDERR->printflush((caller(0))[3] . " - Unknown identifier. Cannot establish fifo listener\n");
            $status |= FIFO_ERR;
        }
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
    my $self = shift(@_);
    my %args;
    my $client;
    my $fh;
    my $status = EXIT_SUCCESS;
    use constant SOCKET_ERROR => 0b01;
    use constant LOG_FILE_ERROR => 0b10;

    # Fetch arguments
    %args = @_;
    # Default values
    $args{'log_level'} = defined($args{'log_level'}) ? $args{'log_level'} : LOG_INFO;
    $args{'message'} = defined($args{'message'}) ? $args{'message'} : undef;
    $args{'socket'} = defined($self->get_socket()) ? $self->get_socket() : defined($args{'socket'}) ? $args{'socket'} : LogUtil::get_default_socket();
    $args{'fifo'} = defined($self->get_fifo()) ? $self->get_fifo() : defined($args{'fifo'}) ? $args{'fifo'} : LogUtil::get_default_fifo();
    $args{'log_file'} = defined($self->get_log_file()) ? $self->get_log_file() : defined($args{'log_file'}) ? $args{'log_file'} : LogUtil::get_default_log_file();
    foreach my $key (keys(%args)) {
        if (defined($args{$key})) {
            chomp($args{$key});
        }
    }

    # Check if log_level was passed correctly
    return if (!defined($args{'log_level'}) || !looks_like_number($args{'log_level'}));

    # Check if message was provided
    return if (!defined($args{'message'}));

    # Check if log level is valid
    return if ($args{'log_level'} != LOG_DEBUG &&
        $args{'log_level'} != LOG_INFO &&
        $args{'log_level'} != LOG_NOTICE &&
        $args{'log_level'} != LOG_WARNING &&
        $args{'log_level'} != LOG_ERR &&
        $args{'log_level'} != LOG_CRIT &&
        $args{'log_level'} != LOG_ALERT &&
        $args{'log_level'} != LOG_EMERG);

    # If a log file is defined, append to it
    if (defined($args{'log_file'})) {
        if (-w dirname($args{'log_file'})) {
            if (open($fh, '>>', $args{'log_file'})) {
                print($fh localtime() . " • " . LogUtil::loglevel_to_bareword($args{'log_level'}) . " • " . $args{'message'} . "\n");
                close($fh);
            } else {
                warn($!);
                $status |= LOG_FILE_ERROR;
            }
        } else {
            STDERR->printflush((caller(0))[3] . " - Unable to write to " . $args{'log_file'} . "\n");
            $status |= LOG_FILE_ERROR;
        }
    }

    # Try to connect to socket
    if (defined($args{'socket'})) {
        if (defined($client = IO::Socket::UNIX->new(Type => SOCK_STREAM, Peer => $args{'socket'}))) {
            $client->autoflush(1);
            $client->send($args{'log_level'} . "\n");
            $client->send($args{'message'});
            $client->shutdown(SHUT_RDWR);
            $client->close();
            $client = undef;
        } else {
            STDERR->printflush((caller(0))[3] .  " - Unable to connect to " . $args{'socket'} . "\n");
            $status |= SOCKET_ERROR;
        }
    }

    # Check if neither a server socket or log file are used
    if (!defined($args{'log_file'}) && (!defined($args{'socket'}))) {
        STDERR->printflush((caller(0))[3] . " - No log destinations are configured. Sending subsequent messages to $default_log_file\n");
        return LogUtil::send(log_level => $args{'log_level'}, message => $args{'message'});
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
