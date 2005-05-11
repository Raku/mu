
use v6;
module Perl::MetaModel-0.0.1;

use Perl::MetaClass;
use Perl::MetaProperty;
use Perl::MetaAssoc;
use Perl::MetaMethod;

sub true() { (1==1) }
sub false() { (1==0) }

our %Classes is export;

# hmm, what objects are we going to have in our metamodel?

# note that there is no namespace conflict with "application" classes;
# they exist on different levels.
%Classes<$_> = Perl::MetaClass->new($_)
    for <object package module role class attribute method signature
         function constraint vtype subtype tuple>;

# then describe their relationships
%C := %Classes; # I ❦ P6

# everything is an object (until defined otherwise)
for %C.keys -> $class {
    %C<$class>.clsIsa(%C<object>) unless $class eq "object";
}

# note: this model that follows is really the biggest unknown, and
# arguably what all the other code is being built to support the
# development of.

# parts of this have effectively already been prototyped in parts of
# the Pugs source!

# from S06, modules, roles, classes are all packages.  I'll go a
# little further and point out that a Class and a Role are probably
# close enough to be an ISA relationship, and that a role builds on a
# module too.
%C<module>.clsIsa(%C<package>);
%C<role>.clsIsa(%C<module>);
%C<class>.clsIsa(%C<role>);

# methods are just functions, as are constraints
%C<method>.clsIsa(%C<function>);
%C<constraint>.clsIsa(%C<function>);

# a package has a collection of symbols that refer to variables
# (objects).  these are keyed by the sigil and variable name; you
# could say that there are actually three relationships..

my $sym_ma1 = Perl::MetaAssoc->new(%C<package>);
my $sym_ma2 = Perl::MetaAssoc->new(%C<object>);
$sym_ma1.assocPair($sym_ma2);
$sym_ma1.assocKeyed(true);
$sym_ma2.assocPair($sym_ma1);
$sym_ma2.range([ 0, 1 ]);

%C<package>.clsAssocs_insert ( symbols => $sym_ma1 );
%C<object>.clsAssocs_insert ( package => $sym_ma2 );

# clearly, the above mess is a bit too much to type every time :)

# a Role consists of a series of method signatures, and a list of
# parent roles
%C<signature>.clsProperties
    ( Perl::MetaProperty->new(<arguments>)
    );

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

=item I<Glenn Branca - Symphony No. 6 (Devil Choir At The Gates of Heaven)>

=item I<Infected Mushroom - Classical Mushroom>

=back

=head1 AUTHORS

Sam Vilain

Stevan Little E<lt>stevan@iinteractive.comE<gt>

=cut
