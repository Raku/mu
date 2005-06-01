
class Class;

method new(Type $base, Str ?$name) returns Class {

}

=head1 NAME

Class - Entities in a program's model (M1)

=head1 SYNOPSIS

 # class SomeClass { method ... { ... } }
 my $class = Class.new("SomeClass");
 $class.addMethod(Method.new(...))
 $class.apply;

 # my SomeClass $foo does Foo;
 my $role = Role.new("Foo");
 my $newclass = Class.new($class);
 $newclass.does($role);
 $newclass.apply;

=head1 DESCRIPTION

This object represents a Class.  It is what is created when you
declare a new class.

=cut

