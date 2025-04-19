#!/bin/env perl

package Main;

use strict;
use warnings;

sub main {
  my $exit_code = 0;
  my $total_grade = 1.0;
  my $current_relative_grade = 0.9592;
  my $course_progress = 0.85; # Total earnable credit at this point in the course
  my $current_grade = $current_relative_grade * $course_progress;
  my $desired_grade = 0.9; # 0.8 for B, 0.9 for A
  my $required_effort = undef;

  # Data pre-processing
  $course_progress = Main::clamp(max => $total_grade, val => $course_progress);
  
  # No amount of effort is going to change your grade if the course is complete
  if ($course_progress - $total_grade >= 0.0) {
    STDERR->print("Your final grade is $current_grade/$total_grade\n");
    return $exit_code;
  }

  # Remember how to do Algebra 1? :)
  # "The grade I need to get on the remaining coursework" + "The grade I currently have" = "The grade I want to have"
  # ($total_grade - $course_progress) * $required_effort + $current_grade = $desired_grade
  # ($total_grade - $course_progress) * $required_effort = $desired_grade - $current_grade
  # $required_effort = ($desired_grade - $current_grade) / ($total_grade - $course_progress)

  $required_effort = ($desired_grade - $current_grade) / ($total_grade - $course_progress);

  my $summary = << 'EOF';
Course progress: @COURSE_PROGRESS@/@TOTAL_GRADE@ complete
Current absolute grade: @CURRENT_GRADE@
Current grade relative to course progress: @CURRENT_RELATIVE_GRADE@
Desired grade: @DESIRED_GRADE@/@TOTAL_GRADE@
Required effort: @REQUIRED_EFFORT@

On the remainder of the coursework, you need to earn an AVERAGE grade of @REQUIRED_EFFORT@ to earn a grade of @DESIRED_GRADE@/@TOTAL_GRADE@.
EOF
  
  $summary =~ s/\@CURRENT_GRADE\@/${current_grade}/g;
  $summary =~ s/\@CURRENT_RELATIVE_GRADE\@/${current_relative_grade}/g;
  $summary =~ s/\@COURSE_PROGRESS\@/${course_progress}/g;
  $summary =~ s/\@TOTAL_GRADE\@/${total_grade}/g;
  $summary =~ s/\@DESIRED_GRADE\@/${desired_grade}/g;
  $summary =~ s/\@REQUIRED_EFFORT\@/${required_effort}/g;

  STDERR->write($summary);
  if ($required_effort < 0) {
    STDERR->write("\nPutting in negative effort isn't practically possible... You could do nothing for the rest of the course and still get the grade you want. I wouldn't recommend that, though. :)\n");
  }

  return $exit_code;
}

sub clamp {
  my %args = @_;
  $args{min} = 0.0 if !defined($args{min}); # Default to 0.0 min
  $args{max} = 1.0 if !defined($args{max}); # Default to 1.0 max
  
  if (!defined($args{val})) {
    warn("No value provided");
    return undef;
  }

  if ($args{val} < $args{min}) {
    return $args{min};
  } elsif ($args{val} > $args{max}) {
    return $args{max};
  } else {
    return $args{val};
  }
}

1;

exit(Main::main());
