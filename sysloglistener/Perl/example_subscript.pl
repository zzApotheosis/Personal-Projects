#!/usr/bin/perl -w

use strict;
use warnings;
use IO::Socket::UNIX;
use lib "lib";
use LogUtil;

LogUtil::send(MESSAGE => "Test message from example_subscript, socket defined", SOCKET => $ENV{'LOGUTIL_SOCK'});
LogUtil::send(MESSAGE => "Test message from example_subscript, no socket defined");
LogUtil::send(MESSAGE => "Test message from example_subscript, log_file defined as test.log", LOG_FILE => "test.log");
