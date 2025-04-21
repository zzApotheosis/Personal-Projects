package OtherPackage::OtherModule1;

use strict;
use warnings;

sub load {
  STDERR->print("Reached " . (caller(0))[3] . "\n");
}

1;
