# HashUtil Class
package HashUtil;

# Class information
our $VERSION = "0.0.1-20210825";

# Constants
use constant DEFAULT_ALGO => 'sha256';

# Imports
use strict;
use warnings;
use POSIX;
use Scalar::Util qw(looks_like_number);
use File::Basename;
use Digest::SHA;
use JSON;
use File::Find;
use Term::ANSIColor;
use IO::File;

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
    $args{algo} = HashUtil::get_default_algo() if !defined($args{algo});
    $args{data} = undef if !defined($args{data});
    $args{file_list_ref} = [] if !defined($args{file_list_ref});
    
    # Initialize object
    $self = {
        _algo => $args{algo},
        _data => $args{data},
        _file_list_ref => $args{file_list_ref},
    };

    # Instantiate object
    bless($self, $class);

    # Return new object
    return $self;
}

#
# walk(algo => $algo, basedir => $basedir)
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
    my %find_opts;
    my %data;
    my %digests;

    # Fetch arguments
    %args = @_;
    $args{algo} = $self->get_algo() if !defined($args{algo});
    $args{basedir} = getcwd() if !defined($args{basedir});
    
    # Define a subroutine for the find() function to use
    $find_func = sub {
        # Define subroutine variables
        my $digest;
        my $sha;
        my $fh = IO::File->new();
        
        # Skip if it's a directory
        return if (-d $_);
        
        # Announce this iteration
        STDOUT->printflush("Processing $File::Find::name\n");
        
        # Open the file for reading
        if ($fh->open("$File::Find::name", 'r')) {
            # Initialize SHA object
            $sha = Digest::SHA->new($args{algo});
            
            # Compute the digest of the given file
            $sha->addfile($fh);
            $digest = $sha->hexdigest();
            $digests{$File::Find::name} = $digest;

            # Close filehandle
            $fh->close();
        } else {
            warn($!);
        }
    };

    # Walk the basedir
    chdir($args{basedir});
    %find_opts = (no_chdir => 1, wanted => $find_func);
    find(\%find_opts, ".");
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
# This subroutine will read a given file and attempt to decode it as a JSON. If the decoding
# succeeds, this subroutine adds the JSON data to this object's data attribute.
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
# export_data()
#
# This subroutine will read the current object's data attribute, decode the JSON data,
# and return the hash reference.
#
sub export_data {
    # Define subroutine variables
    my $self = shift(@_);
    my $hash_ref;
    
    # Check if data is defined
    if (!defined($self->get_data())) {
        return undef;
    }
    
    # Decode the data
    $hash_ref = decode_json($self->get_data());

    # Return it
    return(%{$hash_ref});
}

#
# add_file(@_)
#
# This subroutine will add the given list of files to this object's internal file list array.
#
# Args:
#   @_ • An arbitrarily long list of files to add to the internal file list array
#
sub add_file {
    # Define subroutine variables
    my $self = shift(@_);
    
    # Check for arguments
    if (!scalar(@_)) {
        return;
    }

    # Check if the file list ref is undefined
    if (!defined($self->get_file_list_ref())) {
        $self->set_file_list_ref([]);
    }

    # Add the arguments to this object's file list
    foreach (@_) {
        push(@{$self->get_file_list_ref()}, $_);
    }
}

#
# digest(algo => $algo)
#
# This subroutine will loop through all the known files in the internal file list array
# and compute their digest. The hash algorithm may be overridden by using the $algo argument.
# If it is not provided, this subroutine will default to the current object's defined algorithm.
# If all else fails, the class default algorithm is used.
#
# Args:
#   $algo • The SHA algorithm to use. If not specified, defaults to the object's current recorded algorithm
#
sub digest {
    # Define subroutine variables
    my $self = shift(@_);
    my %args;
    my $sha;
    my %digests;
    my %data;
    my $json;
    
    # Fetch arguments
    %args = @_;
    $args{algo} = $self->get_algo() if !defined($args{algo});
    
    # Check if the selected algorithm is defined
    if (!defined($args{algo})) {
        $args{algo} = HashUtil::get_default_algo();
    }
    
    # Loop through all current files
    for (my $i = 0; $i < scalar(@{$self->get_file_list_ref()}); $i++) {
        $sha = Digest::SHA->new($args{algo});
        if ($sha->addfile(${$self->get_file_list_ref()}[$i])) {
            $digests{${$self->get_file_list_ref()}[$i]} = $sha->hexdigest();
        } else {
            warn($!);
        }
    }
    
    # Record the data for the JSON
    $data{sha_algorithm} = $args{algo};
    $data{digests} = \%digests;
    
    # Create a JSON from the data
    $json = encode_json(\%data);

    # Set the data field
    $self->set_data($json);
}

#
# clear()
#
# This subroutine clears the file list array and the data attribute for a fresh start
#
sub clear {
    # Define subroutine variables
    my $self = shift(@_);
    
    # Reset
    $self->set_algo(HashUtil::get_default_algo());
    $self->set_data(undef);
    $self->set_file_list_ref([]);
}

#
# verify()
#
# This subroutine verifies the current data in memory against the real files on the filesystem.
#
# NOTE: This subroutine assumes that all files referenced in the data in memory exist relative
# to the current working directory. I.E. if there is a digest in memory whose file exists at
# "./path/to/file.txt", then the real file is expected to exist at "./path/to/file.txt". To be
# safe, use chdir() to the dirname of the JSON file before calling this subroutine.
#
# Args:
#   $basedir • The directory to chdir()
#
sub verify {
    # Define subroutine variables
    my $self = shift(@_);
    my %args;
    my $initial_cwd = getcwd();
    my %data;
    my %digests;
    my $sha;
    my $algo;
    my @file_list;
    my $computed_digest; # Computed from the filesystem
    my @verified_files;
    my @fh_errors;
    my @missing_files;
    my @mismatch_list;
    my $fh = IO::File->new();
    
    # Fetch arguments
    %args = @_;
    $args{basedir} = getcwd() if !defined($args{basedir});
    
    # Check if there is any data to work with
    if (!defined($self->get_data())) {
        warn("Data undefined");
        return;
    }
    
    # Check if the basedir exists
    if (! -d $args{basedir}) {
        warn($!);
        return;
    }
    
    # Change to the basedir
    chdir($args{basedir});
    
    # Parse JSON data
    %data = %{decode_json($self->get_data())};
    %digests = %{$data{digests}};
    $algo = $data{sha_algorithm};
    @file_list = sort(keys(%digests));
    
    # Loop through all digests and compare them
    STDOUT->printflush("********** Verifying Data Integrity ***********\n");
    for (my $i = 0; $i < scalar(@file_list); $i++) {
        STDOUT->printflush("* " . color('blue') . "$file_list[$i]" . color('reset') . " ... ");
        if (! -e $file_list[$i]) {
            STDOUT->printflush(color('yellow') . "Missing\n" . color('reset'));
            push(@missing_files, $file_list[$i]);
            next;
        }
        $sha = Digest::SHA->new($algo);
        if ($fh->open($file_list[$i], 'r')) {
            $sha->addfile($fh);
            $computed_digest = $sha->hexdigest();
            if ($computed_digest eq $digests{$file_list[$i]}) {
                STDOUT->printflush(color('green') . "Verified\n" . color('reset'));
                push(@verified_files, $file_list[$i]);
            } else {
                STDOUT->printflush(color('red') . "Mismatch\n" . color('reset'));
                push(@mismatch_list, $file_list[$i]);
            }
            $fh->close();
        } else {
            STDOUT->printflush(color('red') . "$!\n" . color('reset'));
            push(@fh_errors, $file_list[$i]);
        }
    }
    STDOUT->printflush("***********************************************\n");
    STDOUT->printflush("\n");
    
    # Change back to initial cwd
    chdir($initial_cwd);
    
    # Print results
    STDOUT->printflush("***** Data Integrity Verification Results *****\n");
    STDOUT->printflush("* Total files processed: " . color('blue') . scalar(@file_list) . color('reset') . "\n");
    STDOUT->printflush("* Files successfully verified: " . color('green') . scalar(@verified_files) . "\n" . color('reset'));
    STDOUT->printflush("* Missing files: " . (scalar(@missing_files) ? color('yellow') : color('green')) . scalar(@missing_files) . "\n" . color('reset'));
    foreach (@missing_files) {
        STDOUT->printflush("* " . color('yellow') . "$_\n" . color('reset'));
    }
    STDOUT->printflush("* Files with mismatches: " . (scalar(@mismatch_list) ? color('red') : color('green')) . scalar(@mismatch_list) . "\n" . color('reset'));
    foreach (@mismatch_list) {
        STDOUT->printflush("* " . color('red') . "$_\n" . color('reset'));
    }
    STDOUT->printflush("* Filehandle errors: " . (scalar(@fh_errors) ? color('red') : color('green')) . scalar(@fh_errors) . "\n" . color('reset'));
    foreach (@fh_errors) {
        STDOUT->printflush("* " . color('red') . "$_\n" . color('reset'));
    }
    STDOUT->printflush("***********************************************\n");
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

sub set_file_list_ref {
    return if (!defined($_[0]) && !defined($_[1]));
    $_[0]->{_file_list_ref} = $_[1];
}

sub get_algo {
    return $_[0]->{_algo} if defined($_[0]);
}

sub get_data {
    return $_[0]->{_data} if defined($_[0]);
}

sub get_file_list_ref {
    return $_[0]->{_file_list_ref} if defined($_[0]);
}

# End HashUtil Class
1;
