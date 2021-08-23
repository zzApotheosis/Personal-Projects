# HashUtil Class
package HashUtil;

# Class information
our $VERSION = "0.0.1-20210819";

# Constants
use constant DEFAULT_ALGO => 256;

# Imports
use strict;
use warnings;
use POSIX;
use Scalar::Util qw(looks_like_number);
use File::Basename;
use Digest::SHA;
use JSON;
use File::Find;

# Static fields
my $default_algo = HashUtil::DEFAULT_ALGO;

#
# set_default_algo()
#
# A static subroutine to set the class default log file.
#
sub set_default_algo {
    return if (!defined($_[0]));
    $default_algo = $_[0];
}

#
# get_default_algo()
#
# A static subroutine to get the class default log file.
#
sub get_default_algo {
    return $default_algo;
}

# TODO: Any class subroutines will go here

#
# new()
#
# This is the constructor for this class. It accepts named arguments only, or no
# arguments at all.
#
sub new {
    # Define subroutine variables
    my %args;
    my $class = shift(@_);
    my $self;

    # Fetch args
    %args = @_;
    $args{algo} = defined($args{algo}) ? $args{algo} : HashUtil::get_default_algo();
    $args{data} = defined($args{data}) ? $args{data} : undef;
    
    # Initialize object
    $self = {
        _algo => $args{algo},
        _data => $args{data},
    };

    # Instantiate object
    bless($self, $class);

    # Return new object
    return $self;
}

#
# hash_walk(algo => $algo, basedir => $basedir)
#
# This subroutine will walk a target directory and its subdirectories and create a JSON-encoded
# digest list of every file in the target directory and its subdirectories.
# 
# Args:
#   $algo • The SHA algorithm to use
#   $basedir • The target directory to walk
#
sub hash_walk {
    # Define subroutine variables
    my $self = shift(@_);
    my %args;
    my $initial_cwd = getcwd();
    my $json;
    my $base_dir;
    my $find_func;
    my %data;
    my %digests;

    # Fetch arguments
    %args = @_;
    $args{algo} = defined($args{algo}) ? $args{algo} : $self->get_algo();
    $args{basedir} = defined($args{basedir}) ? $args{basedir} : getcwd();
    
    # Define a subroutine for the find() function to use
    $find_func = sub {
        # Define subroutine variables
        my $digest;
        my $sha;
        
        # Skip if it's a directory
        return if (-d $_);
        
        STDOUT->printflush("Processing $File::Find::name\n");
        # Initialize SHA object
        $sha = Digest::SHA->new($args{algo});
        
        # Compute the digest of the given file
        $sha->addfile($File::Find::name);
        $digest = $sha->hexdigest();
        $digests{$File::Find::name} = $digest;
    };

    # Walk the basedir
    chdir($args{basedir});
    find($find_func, ".");
    chdir($initial_cwd);
    
    # Record the data for the JSON
    $data{sha_algorithm} = $args{algo};
    $data{digests} = \%digests;
    
    # Create a JSON from the data
    $json = encode_json(\%data);

    # Set the data field
    $self->set_data($json);
}

#
# write_data(outfile => $outfile)
#
# Args:
#   $outfile • The output file to write the data to
#
# Exceptions:
#   • No data available to write
#   • Undefined outfile
#   • Failed to open output file for writing
#
sub write_data {
    # Define subroutine variables
    my $self = shift(@_);
    my %args;
    my $fh;
    
    # Fetch arguments
    %args = @_;
    $args{outfile} = defined($args{outfile}) ? $args{outfile} : undef;
    
    # Check if there's any data to write at all
    if (!defined($self->get_data)) {
        die($!);
    }
    
    # Check for outfile
    if (!defined($args{outfile})) {
        die($!);
    }
    
    # Open outfile for writing
    open($fh, '>', $args{outfile}) or die($!);
    $fh->printflush($self->get_data());
    close($fh);
}

#
# read_data(infile => $infile)
#
# Args:
#   $infile • The input JSON file to read
#             This JSON file must contain two keys: sha_algorithm, digests
#             The digests key must contain a list of input files and their digests
#
# Exceptions:
#   • Undefined input file
#   • Input file does not exist
#   • Error occured while opening input file
#   • Input data failed to parse as JSON
#
sub read_data {
    # Define subroutine variables
    my $self = shift(@_);
    my %args;
    my $fh;
    my $in_data;

    # Fetch arguments
    %args = @_;
    $args{infile} = defined($args{infile}) ? $args{infile} : undef;
    
    # Check for infile
    if (!defined($args{infile})) {
        die($!);
    }

    # Check if file exists
    if (-e $args{infile}) {
        # Read the file
        open($fh, '<', $args{infile}) or die($!);
        $in_data = <$fh>;
        close($fh);
        chomp($in_data);
        
        # Attempt parsing the input data
        decode_json($in_data) or die($!);
        
        # At this point, the data is likely perfectly valid JSON data
        $self->set_data($in_data);
    } else {
        die($!);
    }
}

#
# Setters and getters
#
sub set_algo {
    return if (!defined($_[0]) && !defined($_[1]));
    $_[0]->{_algo} = $_[1];
}

sub set_data {
    return if (!defined($_[0]) && !defined($_[1]));
    $_[0]->{_data} = $_[1];
}

sub get_algo {
    return $_[0]->{_algo} if defined($_[0]);
}

sub get_data {
    return $_[0]->{_data} if defined($_[0]);
}

# End HashUtil Class
1;
