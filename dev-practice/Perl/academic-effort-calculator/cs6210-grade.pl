#!/bin/env perl

package Main;

use strict;
use warnings;
use FindBin;
use Cwd;

my $exec_name = "$FindBin::RealScript";
my $exec_path = "$FindBin::RealBin/$FindBin::RealScript";
my $exec_dir = "$FindBin::RealBin";
my $original_cwd = getcwd();

sub main {
    my $exit_code = 0;

    my $course = Course->new(name => 'CS6210');
    $course->add_item(Item->new(name => 'Pre-lab', weight => 0.02, score => 15.0, total => 16.0));
    $course->add_item(Item->new(name => 'Homework 0', weight => 0.03, score => 27.5, total => 35.0));
    $course->add_item(Item->new(name => 'Project 1', weight => 0.12, score => 17.75, total => 18.0));
    $course->add_item(Item->new(name => 'Project 2', weight => 0.12, score => 10.0, total => 10.0));
    $course->add_item(Item->new(name => 'Project 3', weight => 0.12, score => 12.0, total => 12.0));
    $course->add_item(Item->new(name => 'Project 4', weight => 0.12, score => 12.0, total => 12.0));
    $course->add_item(Item->new(name => 'Class Participation', weight => 0.05, score => 2.0, total => 5.0));
    $course->add_item(Item->new(name => 'Exam 1', weight => 0.16, score => 72.75, total => 106.0));
    $course->add_item(Item->new(name => 'Exam 2', weight => 0.14, score => 59.5, total => 80.0));
    $course->add_item(Item->new(name => 'Exam 3', weight => 0.12, score => 61.5, total => 68.0));
    #$course->add_item(Item->new(name => 'Extra credit', weight => 0.005, score => 1, total => 1));

    $course->validate();
    $course->calculate();
    $course->report();

    return $exit_code;
}

1;

package Course;

use strict;
use warnings;

sub new {
    my $class = shift(@_);
    my %args = @_;
    $args{items} = [] if !defined($args{items});
    my $self = {
        items => $args{items},
    };
    bless($self, $class);
    return $self;
}

sub add_item {
    my $self = shift(@_);
    my $item = shift(@_);
    push(@{$self->{items}}, $item);
}

sub validate {
    my $self = shift(@_);
    my $weight_total = 0;
    for (my $i = 0; $i < scalar(@{$self->{items}}); $i++)
    {
        my $item = ${$self->{items}}[$i];
        $weight_total += $item->{weight};
    }

    # Weight total must be minimum of 1
    die('Invalid course') if ($weight_total < 1.0);
}

sub calculate {
    my $self = shift(@_);
    my $grade = 0;
    for (my $i = 0; $i < scalar(@{$self->{items}}); $i++)
    {
        my $item = ${$self->{items}}[$i];
        $grade += $item->{weight} * ($item->{score} / $item->{total});
    }
    $self->{grade} = $grade;
}

sub report {
    my $self = shift(@_);
    STDERR->write("Course grade: " . $self->{grade} . "\n");
}

1;

package Item;

use strict;
use warnings;

sub new {
    my $class = shift(@_);
    my %args = @_;
    my $self = {
        name => $args{name},
        weight => $args{weight},
        score => $args{score},
        total => $args{total},
    };
    bless($self, $class);
    return $self;
}

1;

exit(Main::main());

