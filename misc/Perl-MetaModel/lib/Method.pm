
class Method--Perl6;

has Code $.code;

=head1 NAME

Method - functions on classes

=head1 SYNOPSIS



=head1 DESCRIPTION

Methods are bindings of Code objects to a particular Type.

=over

=item *

A method's Code property usually has a single invocant typed to the
Role it is attached to (however, this is a trend and not a rule).

=item *

A method is bound to a compiled class (a L<Type>), which is used for
dispatching private methods.

=back

Normally, when you declare a Method on a Class, it is bound to that
Class' C<Type> object immediately; however, in the case of a Role, it
is not bound to a type until the role is included in the class.

This has the effect of making C<Role> objects appear to "flatten" in
the classes which they are included; unlike the "mix-in" effect of
adding more superclasses to a Class, all of the methods that are
included via roles will be able to access the Class' private
attributes.  This is "early" method binding.

=head1 NOTES

Perhaps this class should be called MethodBinding.

=cut

