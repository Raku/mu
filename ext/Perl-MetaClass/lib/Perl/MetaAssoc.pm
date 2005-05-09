
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
# ∃ MetaClass A, MetaAssoc C : A.clsCats ∋ C ↔ C.catClass = A
# 
# ∃ MetaAssoc C₁, C₂ : C₁.catPair = C₂ ↔ C₂.catPair = C₁
# 
# -- can't be composite both ways
# 
# ∃ MetaAssoc C₁, C₂ : C₁.catPair = C₂ ∧ C₁.catIsComposite
#      → ¬(C₂.catIsComposite)
# 
# -- this seems the simplest way to specify complementary categories
# 
# ∃ MetaAssoc C₁, C₂, MetaClass M₁, M₂
#    : C₁.catPair = C₂ ∧ C₁.catClass = M₁ ∧ C₂.catClass = M₂
#    → (   ∃ M₁.clsCats{C₂.catCompanion}
#        ∧ ∃ M₂.clsCats{C₁.catCompanion}
#        ∧ M₁.clsCats{C₂.catCompanion}[1] = C₁
#        ∧ M₂.clsCats{C₁.catCompanion}[1] = C₂
#        ∧ M₁.clsCats{C₂.catCompanion}[0] = M₂.clsCats{C₁.catCompanion}[0]
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

