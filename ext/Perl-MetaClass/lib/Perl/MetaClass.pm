    
use v6;
module Perl::MetaClass-0.0.1;

use Hack::Instances;

use Perl::MetaProperty;
#use Perl::MetaMethod;
#use Perl::MetaAssoc;

sub Perl::MetaClass::new(Str $name) returns Str is export {
    my $id = make_instance("Perl::MetaClass", { 
        'name'       => $name,
        'super'      => undef,
        'subclasses' => [],
        'properties' => hash(),
        'methods'    => hash(),
        'assoc'      => hash(),
    });
    return $id;
}

sub clsName(Str $inv: Str ?$name) returns Str {
    my %self := get_instance($inv, "Perl::MetaClass");
    %self<name> = $name if $name.defined;
    return %self<name>;
}

# NOTE: 
# the next 2 methods enforce the rule;
#   ∃ MetaClass A, B | A.clsSuper = B ↔ A ∈ B.clsSubClasses
# which in english means:
#   A is a superclass of B and A is found within B's list of subclasses

sub clsSuper(Str $inv: Str ?$super) returns Str {
    my %self := get_instance($inv, "Perl::MetaClass");
    if $super.defined {
        # NOTE:
        # we enforce the following rule on superclasses:
        # - it must be an instance of Perl::MetaClass
        ($super.instance_isa('Perl::MetaClass'))
            || die "Super class must be a Perl::MetaClass instance";        
        %self<super> = $super;
        # now add the invocant to the super's subclasses
        $super.clsSubClasses($inv);
    }
    return %self<super>;
}

# XXX -- we need to enforce Set like behavior here
# maybe with a hash? keyed by subclass name?

sub clsSubClasses(Str $inv: Array *@subclasses) returns Array {
    my %self := get_instance($inv, "Perl::MetaClass");
    if @subclasses {
        # NOTE:
        # enforce the following rules on all @subclasses:
        # - they are instances of Perl::MetaClass
        # - they're superclass is our invocant
        for @subclasses -> $subclass {
            ($subclass.instance_isa('Perl::MetaClass'))
                || die "Sub class must be a Perl::MetaClass instance (got: '$subclass')"; 
            ($subclass.clsSuper() && $subclass.clsSuper().clsName() eq $inv.clsName())
                || die "Sub class's superclass must be the invocant (got: '{ $subclass.clsSuper() }')";             
        } 
        # NOTE: 
        # this is kind of ugly, but I get a 
        # "can't modify constant" error otherwise
        my @inv_subclasses = %self<subclasses>;
        %self<subclasses> = [ @inv_subclasses, @subclasses ];    
    }
    return %self<subclasses>;
}

# XXX -- what do we do about visibility here?
# is it a property of the MetaProperty? or is
# it a property of the relationship with the
# MetaClass? 

sub clsProperties(Str $inv: Array *@properties) returns Hash {
    my %self := get_instance($inv, "Perl::MetaClass");
    if @properties {
        for @properties -> $key, $value {
            ($value.instance_isa('Perl::MetaProperty'))
                || die "Property values must be a Perl::MetaProperty instance (got: '$value')";             
            my %props = %self<properties>;
            %props{$key} = $value;
            %self<properties> = \%props;        
        }
    }
    return %self<properties>;
}

sub clsMethods(Str $inv: Hash ?%methods) returns Hash {
    my %self := get_instance($inv, "Perl::MetaClass");
    %self<methods> = \%methods if %methods.defined;
    return %self<methods>;
}

sub clsAssocs(Str $inv: Hash ?%assocs) returns Hash {
    my %self := get_instance($inv, "Perl::MetaClass");
    %self<assocs> = \%assocs if %assocs.defined;
    return %self<assocs>;
}


=pod

=head1 NAME

Perl::MetaClass - A meta-model for Perl Classes

=head1 SYNOPSIS
  
  my $superclass = Perl::MetaClass::new('Foo');
  my $class = Perl::MetaClass::new('Foo::Bar');  
  
  $class.clsSuperClass($superclass);

=head1 DESCRIPTION

A Perl::MetaClass object is an object which holds objects that
describe the Perl 6 Class system (or, potentially, any other Class
system too).

=head1 PRIOR ART

In the T2 CPAN module, the Class::Tangram module is behaving as a
Class Meta-Model, and T2 is describing the Class Model.  A set of T2
objects represent a set of Classes.

However, the T2 module only represents a classical single inheritance
model without interfaces, so cannot represent everything that the
Roles-based model of Perl 6 will.

=head1 RECOMMENDED LISTENING

=over 4

=item I<Squarpusher - Alive in Japan>

=back

=head1 METHODS

=over 4

=item B<Perl::MetaClass::new ($name)>

=item B<clsName($inv: ?$name)>

=item B<clsSuperClass($inv: ?$superclass)>

=item B<clsSubClasses($inv: *@subclasses)>

=item B<clsProperties($inv: ?%properties)>

=item B<clsMethods($inv: ?%methods)>

=item B<clsAssocs($inv: ?%assocs)>

=back

=head1 AUTHORS

Sam Vilain

Stevan Little E<lt>stevan@iinteractive.comE<gt>

=cut
