
class Class--Perl6;

is Role--Perl6;

has Type $.type;

method subclass(Role $base, Str ?$name) returns Class {

}

method isa(Role $class) returns Bool {
    ...
}

=head1 NAME

Class--Perl6 - Entities in a program's model (M1)

=head1 SYNOPSIS

 # class SomeClass { method ... { ... } }
 my $class = Class--Perl6.new("SomeClass");
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

A class is a role with the enhancement that is it I<typable>.

This means;

=over

=item *

You can construct new classes which are of its type.

=item *

It serves as the basis for a method dispatch scope.  Note that methods
differ from normal Code refs only in that they do not have a
dynamically bound scope for private accessors, etc.

=back

=cut

