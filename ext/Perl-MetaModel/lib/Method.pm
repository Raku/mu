
class Method--Perl6 is Code;

has Type $.private;

=head1 NAME

Method - functions on classes

=head1 SYNOPSIS



=head1 DESCRIPTION

Methods are the same as Code objects, with the following distinctions;

=over

=item *

A method usually has a single invocant (however, this is a trend and
not a rule).

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
attributes.

=cut

