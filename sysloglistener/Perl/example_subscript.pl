#!/usr/bin/perl -w

use strict;
use warnings;
use IO::Socket::UNIX;
use lib "lib";
use LogUtil;

my $logutil = new LogUtil(fifo => $ENV{LOGUTIL_FIFO});

$logutil->send(message => "Test message from example_subscript.pl");
