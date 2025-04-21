package OtherPackage::OtherModule3;

use strict;
use warnings;

sub load {
    my $msg = "Reached " . (caller(0))[3];
    STDERR->print("$msg\n");
    my $callback = $_[1];
    $callback->($msg);
}

1;
