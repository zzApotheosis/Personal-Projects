package OtherPackage::OtherModule2;

use strict;
use warnings;

sub load {
  STDERR->print("Reached " . (caller(0))[3] . "\n");
}

1;
