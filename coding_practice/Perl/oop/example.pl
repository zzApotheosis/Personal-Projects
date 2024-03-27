#!/usr/bin/perl

# Define the Person class
package Person;

# Define the constructor
sub new {
    my $class = shift;
    my $self = {
        name => shift,
        age => shift,
        gender => shift
    };
    bless $self, $class;
    return $self;
}

# Define the say_hello method
sub say_hello {
    my ($self) = @_;
    print "Hello, my name is $self->{name}.\n";
}

# Define the Student class
package Student;

# Inherit from the Person class
use base 'Person';

# Define the constructor
sub new {
    my $class = shift;
    my $self = $class->SUPER::new(@_);
    $self->{major} = shift;
    return $self;
}

# Define the say_hello method
sub say_hello {
    my ($self) = @_;
    print "Hello, my name is $self->{name} and I'm a $self->{major} major.\n";
}

# Create a new Person object
my $person = Person->new("John", 25, "Male");

# Call the say_hello method on the Person object
$person->say_hello();

# Create a new Student object
my $student = Student->new("Mary", 20, "Female", "Computer Science");

# Call the say_hello method on the Student object
$student->say_hello();
