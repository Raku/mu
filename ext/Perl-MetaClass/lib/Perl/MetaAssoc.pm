
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

# XXX - we need to follow these (?):
# 
#   ∃ MetaClass A, MetaAssoc C : A.clsAssoc ∋ C ↔ C.assocClass = A
# (meta-class to meta-assoc is one-to-many, but meta-assoc to meta-class is one-to-one) <- am I right here?
# 
#   ∃ MetaAssoc C₁, C₂ : C₁.assocPair = C₂ ↔ C₂.assocPair = C₁
# (this is just bi-directional pairing of the two associations)
# 
# -- can't be composite both ways
# 
# ∃ MetaAssoc C₁, C₂ : C₁.assocPair = C₂ ∧ C₁.assocIsComposite
#      → ¬(C₂.catIsComposite)
# 
# -- this seems the simplest way to specify complementary categories
# 
# ∃ MetaAssoc C₁, C₂, MetaClass M₁, M₂
#    : C₁.assocPair = C₂ ∧ C₁.assocClass = M₁ ∧ C₂.assocClass = M₂
#    → (   ∃ M₁.clsAssoc{C₂.assocCompanion}
#        ∧ ∃ M₂.clsAssoc{C₁.assocCompanion}
#        ∧ M₁.clsAssoc{C₂.assocCompanion}[1] = C₁
#        ∧ M₂.clsAssoc{C₁.assocCompanion}[1] = C₂
#        ∧ M₁.clsAssoc{C₂.assocCompanion}[0] = M₂.clsAssoc{C₁.assocCompanion}[0]
#        )


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

=head1 DESCRIPTION

=head1 AUTHORS

Sam Vilain

Stevan Little E<lt>stevan@iinteractive.comE<gt>

=cut

