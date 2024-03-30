#!/bin/perl

#
# This is a helper script to install, setup, and configure my personal GNU Emacs
# instance. This script will backup existing init files and configure the user's home
# directory with the setup in this repository.
#

# Main Class
package Main;

# Imports
use strict;
use warnings;
use FindBin;
use Cwd;
use POSIX qw(strftime);
use File::Copy;
use File::Path qw(make_path);

# Class Fields
my $exec_name = "$FindBin::RealScript";
my $exec_path = "$FindBin::RealBin/$FindBin::RealScript";
my $exec_dir = "$FindBin::RealBin";
my $original_cwd = getcwd();
my $home_dir = $ENV{HOME};
my $emacs_dir = "$home_dir/.emacs.d";
my $now = strftime("%Y-%m-%d_%H:%M:%S", localtime());
my @backup_targets = ("$home_dir/.emacs",
		      "$home_dir/.emacs.el",
		      "$emacs_dir/eshell/alias");

# Main Subroutine
sub main
{
    # Define subroutine variables
    my $exit_code = 0;

    # Backup existing Emacs files (if they exist)
    Main::backup_existing_emacs();

    # Set up directory structure and symlinks
    make_path("$emacs_dir") or warn($!);
    make_path("$emacs_dir/eshell") or warn($!);
    eval {
	symlink("$exec_dir/init.el", "$emacs_dir/init.el") or warn($!);
	symlink("$exec_dir/init.d", "$emacs_dir/init.d") or warn($!);
    };
    if ($@) {
	warn($!);
    }

    # Copy other files to Emacs directory
    copy("$exec_dir/eshell/alias", "$emacs_dir/eshell/alias") or warn($!);

    # Done
    return $exit_code;
}

# Backup Existing Emacs Subroutine
sub backup_existing_emacs
{
    for (my $i = 0; $i < scalar(@backup_targets); $i++)
    {
	if (-l $backup_targets[$i])
	{
	    unlink($backup_targets[$i]) or warn($!);
	}
	elsif (-e $backup_targets[$i])
	{
	    move($backup_targets[$i], "$backup_targets[$i]-backup-$now") or warn($!);
	}
	else
	{
	    # Looks like we don't have to do anything here :)
	}
    }
}

# End Main Class
1;

# Execute
exit(Main::main());
