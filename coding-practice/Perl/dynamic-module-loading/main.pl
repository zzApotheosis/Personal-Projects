#!/bin/perl

# To be honest, there's probably a much better way to do this
# But for the sake of personal documentation, this is a simple
# example to dynamically load Perl modules at runtime without
# hardcoding Perl module names
#
# NOTE: This obviously introduces the ability to inject arbitrary
# Perl code into this program. Be careful with this approach!!!

package Main;

use strict;
use warnings;
use FindBin;
use Cwd;
use File::Find;

my $exec_name = "$FindBin::RealScript";
my $exec_path = "$FindBin::RealBin/$FindBin::RealScript";
my $exec_dir = "$FindBin::RealBin";
my $original_cwd = getcwd();

use lib "$FindBin::RealBin/lib";

sub main {
    my $exit_code = 0;
    my @modules;
    my @namespaced_modules;

    chdir("$FindBin::RealBin/lib") or die($!);
    find({
	no_chdir => 1,
	wanted => sub {
	    if (-d $File::Find::name) {
		return;
	    }

	    if ($File::Find::name =~ /\.pm$/) {
		my $module = $File::Find::name;
		$module =~ s/^\.\///g;
		push(@modules, $module);
	    }
	},
	 },
	 '.');
    chdir($original_cwd);

    foreach (@modules) {
	require($_);
	
	my $namespaced_module = $_;
	$namespaced_module =~ s/\//::/g;
	$namespaced_module =~ s/\.pm$//g;
	push(@namespaced_modules, $namespaced_module);
    }

    foreach (@namespaced_modules) {
	eval {
	    $_->load(\&Main::callback);
	} or do {
	    warn($!);
	    warn("$_ does not implement load()");
	    next;
	};
    }

    return $exit_code;
}

sub callback {
    my $msg = shift(@_);
    chomp($msg);
    STDERR->write("Received in Main::callback - $msg\n");
}

1;

exit(Main::main());
