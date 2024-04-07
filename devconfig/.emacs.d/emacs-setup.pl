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
use File::Path qw(make_path remove_tree);
use File::Basename;
use File::Find;

# Constants
use constant {
    UNIX => 'unix',
    DOS => 'dos',
};

# Class Fields
my $exec_name = "$FindBin::RealScript";
my $exec_path = "$FindBin::RealBin/$FindBin::RealScript";
my $exec_dir = "$FindBin::RealBin";
my $original_cwd = getcwd();
my $home_dir = undef;
my $emacs_dir = undef;
my $now = strftime("%Y-%m-%d_%H:%M:%S", localtime());
my @backup_targets = ();
my $pkg_dir = undef;
my @other_package_targets = ("https://github.com/jaypei/emacs-neotree",
			     "https://github.com/akermu/emacs-libvterm",
			     "https://github.com/protocolbuffers/protobuf");
my $os_type = undef;

# Main Subroutine
sub main
{
    # Define subroutine variables
    my $exit_code = 0;

    # Initialization
    Main::init();

    # Backup existing Emacs files (if they exist)
    Main::backup_existing_emacs();
    
    # Set up directory structure and symlinks
    if (-d "$emacs_dir")
    {
	remove_tree("$emacs_dir");
    }
    if (-l "$emacs_dir")
    {
	unlink("$emacs_dir");
    }
    eval
    {
	Main::deploy();
    };
    if ($@)
    {
    	warn($!);
    }

    # Download non-ELPA packages (yes, I realize that this is what MELPA is for, but I'm still learning Emacs Lisp so I still have yet to write this functionality within elisp itself)
    Main::download_packages();

    # Done
    return $exit_code;
}

# Initialization Subroutine
sub init
{
    # Define subroutine variables
    my $status = 0;

    # Check OS type
    if ($^O eq 'linux'   ||
	$^O eq 'unix'    ||
	$^O eq 'darwin'  ||
	$^O eq 'bsdos'   ||
	$^O eq 'openbsd' ||
	$^O eq 'solaris')
    {
	$home_dir = $ENV{HOME};
	$os_type = UNIX;
    }
    elsif ($^O eq 'dos'     ||
	   $^O eq 'MSWin32' ||
	   $^O eq 'cygwin')
    {
	$home_dir = $ENV{APPDATA};
	$os_type = DOS;
    }

    # Populate variables
    $emacs_dir = "$home_dir/.emacs.d";
    @backup_targets = ("$home_dir/.emacs",
		       "$home_dir/.emacs.el");
    $pkg_dir = "$emacs_dir/pkgs";

    # Check if prerequisite tools will work before continuing
    STDOUT->printflush("Checking prerequisite tools...\n");
    STDOUT->printflush("Checking git... ");
    $status = system("git --version") >> 8;
    if ($status != 0)
    {
	die("Unable to detect Git installation. Visit https://git-scm.com/");
    }
    STDOUT->printflush("\n");
}

# Backup Existing Emacs Subroutine
sub backup_existing_emacs
{
    for (my $i = 0; $i < scalar(@backup_targets); $i++)
    {
	if (-l $backup_targets[$i])
	{
	    STDOUT->printflush("Unlinking $backup_targets[$i]\n");
	    unlink($backup_targets[$i]) or warn($!);
	}
	elsif (-d $backup_targets[$i])
	{
	    warn("Backup target [$backup_targets[$i]] appears to be a directory. Skipping...");
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

# Deploy Subroutine
sub deploy
{
    if ($os_type eq UNIX)
    {
	# On Unix-like systems, let's just symlink ~/.emacs.d to here. Easy :)
	symlink("$original_cwd", "$emacs_dir") or die($!);
    }
    elsif ($os_type eq DOS)
    {	
	remove_tree($emacs_dir);

	# Recursively copy all files in $original_cwd to the destination
	find(
	    {
		wanted => sub {
		    # Define subroutine variables
		    my $depth;
		    my $file_basename;
		    my $target_dir;
		    
		    # Skip "desktop.ini" dumb Microsoft
		    if (basename($_) eq 'desktop.ini')
		    {
			return;
		    }

		    # Skip directories (we'll re-create the directory structure properly)
		    if (-d $File::Find::name)
		    {
			return;
		    }

		    # Mindepth 1
		    $depth = $File::Find::name =~ tr/\///; # tr is the transliteration operator. See https://perldoc.perl.org/perlop#Quote-Like-Operators
		    if ($depth < 1)
		    {
			return;
		    }

		    # Re-format the file name (I don't want the leading "./")
		    $file_basename = '';
		    if ($File::Find::name =~ /^\.\/(.*)$/)
		    {
			$file_basename = $1;
		    }
		    else
		    {
			warn("Cannot determine stripped filename for $File::Find::name");
		    }

		    # Determine target directory
		    $target_dir = '';
		    if (dirname($file_basename) eq '.')
		    {
			$target_dir = $emacs_dir
		    }
		    else
		    {
			$target_dir = $emacs_dir . "/" . dirname($file_basename);
		    }
		    if (! -d $target_dir)
		    {
			make_path($target_dir) or warn($!);
		    }

		    # Copy file to emacs_dir
		    copy($File::Find::name, "$target_dir/" . basename($_)) or warn($!);
		},
		no_chdir => 1
	    }, '.');
    }
    else
    {
	die("How did you manage this?");
    }
}

# Download Packages Subroutine
sub download_packages
{
    # Define subroutine variables
    my $repo_basename;

    remove_tree($pkg_dir);
    make_path($pkg_dir);
    chdir($pkg_dir);
    for (my $i = 0; $i < scalar(@other_package_targets); $i++)
    {
	if ($other_package_targets[$i] =~ /\/([^\/]*)$/)
        {
	    $repo_basename = $1;
	}
	else
	{
	    warn($!);
	    next;
	}
        if (-d $repo_basename)
        {
	    remove_tree($repo_basename) or warn($!);
        }
	system("git clone --depth 1 $other_package_targets[$i]");
    }
    chdir($original_cwd);
}

# End Main Class
1;

# Execute
exit(Main::main());
