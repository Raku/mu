
class Class;

has $.name;

method new(Str $name) returns Class {
    $.name = $name;
    return $?SELF;
}

method addMethod(Method $meth) {
    ...
}

method apply() {
    ...
}

=head1 NAME

Class - Entities in a program's model (M1)

=head1 SYNOPSIS

 my $class = Class.new("SomeClass");

 $class.addMethod(Method.new(...))

 $class.apply;

=head1 DESCRIPTION

This object represents a Class.  It is what is created when you
declare a new class.

=cut

