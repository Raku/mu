
use v6;
module Perl::MetaAssoc-0.0.1;

use Hack::Instances;

sub Perl::MetaAssoc::new(Str $class) returns Str is export {
    ($class.instance_isa('Perl::MetaClass'))
            || die "Class must be a Perl::MetaClass instance";
    my $id = make_instance("Perl::MetaAssoc", { 
        'class'       => $class,   # Perl::MetaClass
        'pair'        => undef,    # Perl::MetaAssoc
        'range'       => [0, Inf], # Range
        'isComposite' => (0 == 1), # Bool
        'ordered'     => (0 == 1), # Bool
        'keyed'       => (0 == 1), # Bool
        'companion'   => undef,    # Str
    });
    return $id;
}


#   ∀ MetaAssoc C₁, C₂ : C₁.assocPair = C₂ ↔ C₂.assocPair = C₁
# (this is just bi-directional pairing of the two associations)
#
# -- can't be composite both ways
#
#
# -- this seems the simplest way to specify complementary categories
#
#
# This is the rule that 


sub assocClass(Str $inv: Str ?$class) returns Str {
    my %self := get_instance($inv, "Perl::MetaAssoc");
    if $class.defined {
        ($class.instance_isa('Perl::MetaClass'))
            || die "Class must be a Perl::MetaClass instance";    
        %self<class> = $class;   
    }
    return %self<class>;
}

sub assocPair(Str $inv:) returns Str {
    my %self := get_instance($inv, "Perl::MetaAssoc");
    return %self<pair>;
}

sub assocRange(Str $inv:) returns Array {
    my %self := get_instance($inv, "Perl::MetaAssoc");
    return %self<range>;
}

sub assocIsComposite(Str $inv:) returns Bool {
    my %self := get_instance($inv, "Perl::MetaAssoc");
    return %self<isComposite>;
}

sub assocOrdered(Str $inv:) returns Bool {
    my %self := get_instance($inv, "Perl::MetaAssoc");
    return %self<ordered>;
}

sub assocKeyed(Str $inv:) returns Bool {
    my %self := get_instance($inv, "Perl::MetaAssoc");
    return %self<keyed>;
}

sub assocCompanion(Str $inv:) returns Str {
    my %self := get_instance($inv, "Perl::MetaAssoc");
    return %self<companion>;
}


=pod

=head1 NAME

Perl::MetaAssoc - A meta-model for Perl Classes

=head1 SYNOPSIS

  use Perl::MetaAssoc;

  # represent a one to many relationship between the
  # "Property" and "Class" MetaClasses.  The association is
  # called .properties on the Class end, and .class on the
  # property end.

  my $Property_mc = Perl::MetaClass::new("Property");
  my $Class_mc    = Perl::MetaClass::new("Class");

  $Class_mc.clsAssocs
       (properties => Perl::MetaAssoc::new
           (
             assocOrdered => false,
             assocRange => [0, Inf],
             assocCompanion => "class",
             assocIsComposite => 1,
             assocPair => Perl::MetaAssoc::new
                             ( assocRange => [1, 1],
                               assocClass => $Property_mc )
           )
       );

=head1 DESCRIPTION

A Perl::MetaAssoc represents a fairly abstract concept.  Firstly, it
is a class of the B<meta-model>, which is not C<Class.meta()> objects
at all.  That distinction is explained at L<Perl::MetaClass>.

This class is used to describe I<relationships> between two classes in
the Class model.  For instance, a Class can have any number of defined
methods, and each method can only belong to one class.  These two
comments are frequently viewed as being seperate relationships, but in
fact they are simply different ways of looking at the same
relationship; that a method belongs to exactly one class (or package,
or whatever).

Each side of the relationship is identical to the other; so, in this
metamodel, relationships are represented with I<two> objects - one for
each side.

There is a lot of implicit 'magic' assumed to be happening in the
example.  This magic stems from rules such as the following;

   ∀ MetaClass A, MetaAssoc C : A.clsAssoc ∋ C ↔ C.assocClass = A

This is read as "For every MetaClass A and MetaAssoc C, such that
A.clsAssoc contains C, it is automatically implied that C.assocClass
is A".

We could have written this explicitly:

  my $properties_class_ma = Perl::MetaAssoc->new();
  $Class_mc->clsAssocs("properties" => $properties_class_ma);
  $properties_class_ma->assocClass($Class_mc);

However, this would be needless duplication of expressing the
relationship; we want this all to happen automatically.

This rule is almost identical:

  ∀ MetaAssoc C₁, C₂ : C₁.assocPair = C₂ ↔ C₂.assocPair = C₁

Just by stating that the C<assocPair> for one of the C<MetaAssoc>
objects is the other, we don't need to specify the reverse relation.

Similarly, as we have stated;

  ∀ MetaAssoc C₁, C₂, MetaClass M₁, M₂
     :   C₁.catPair = C₂  ∧ C₁.assocCompanion
       ∧ C₁.catClass = M₁ ∧ C₂.catClass = M₂
     → (   ∃ M₁.clsAssocs{C₂.catCompanion}
         ∧ ∃ M₂.clsAssocs{C₁.catCompanion}
         ∧ M₁.clsAssocs{C₂.catCompanion}[1] = C₁
         ∧ M₂.clsAssocs{C₁.catCompanion}[1] = C₂
         ∧ M₁.clsAssocs{C₂.catCompanion}[0] = M₂.clsAssocs{C₁.catCompanion}[0]
         )

Then simply by specifying that the C<properties> MetaAssoc object's
companion is called "class", this will mean that its pair MetaAssoc
object's name within the "Property" clsAssocs collection is "class".
It also implies that the pair MetaAssoc will have a C<companion>
property of "properties".

Finally, it is implied that the other side of the association cannot
be composite;

  ∀ MetaAssoc C₁, C₂ : C₁.assocPair = C₂ ∧ C₁.assocIsComposite
     → ¬(C₂.catIsComposite)

=head1 AUTHORS

Sam Vilain

Stevan Little E<lt>stevan@iinteractive.comE<gt>

=cut

