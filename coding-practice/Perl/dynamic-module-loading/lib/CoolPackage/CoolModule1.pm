package CoolPackage::CoolModule1;

use strict;
use warnings;
use Exporter;

our @ISA = qw( Exporter );
our @EXPORT_OK = ();

sub load {
  STDERR->print("Reached " . (caller(0))[3] . "\n");
}

1;
