
use v6;
module Perl::MetaModel-0.0.1;

=pod

=head1 NAME

Perl::MetaModel - A meta-model for Perl6's object system

=head1 DESCRIPTION

The Perl::MetaModel modules are the meta-model of the Perl6 object system. 

  Perl::MetaModel term    S12 term     Access from Perl as
  --------------------    ---------    -------------------
  Perl::MetaClass         -            MyClass.meta.meta
  Perl::Class             MetaClass    MyClass.meta
  -                       Class        MyClass

=head1 PRIOR ART

=over 4

=item B<T2 & Class::Tangram>

In the L<T2> CPAN module, the L<Class::Tangram> module is behaving as a
Class Meta-Model, and L<T2> is describing the Class Model.  A set of L<T2>
objects represent a set of Classes.

However, the L<T2> module only represents a classical single inheritance
model without interfaces, so cannot represent everything that the
Roles-based model of Perl 6 will.

=item B<Class::Trait>

L<Class::Trait> uses a semi-meta-model approach. The L<Class::Trait::Config>
object is a meta-model of a Trait, however it is more passive storage
than it is active meta-object. 

=back

=head1 RECOMMENDED LISTENING

Meta-Model Hacking can be a mind-bending exercise, good music helps :)

=over 4

=item I<Squarepusher - Alive in Japan>

=item I<Boogie Down Productions - Ghetto Music: The Blueprint of Hip Hop>

=back

=head1 AUTHORS

Sam Vilain

Stevan Little E<lt>stevan@iinteractive.comE<gt>

=cut