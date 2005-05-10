    
use v6;
module Perl::MetaClass-0.0.1;

use Hack::Instances;

use Perl::MetaProperty;
use Perl::MetaMethod;
use Perl::MetaAssoc;

sub Perl::MetaClass::new(Str $name) returns Str is export {
    my $id = make_instance("Perl::MetaClass", { 
        'name'       => $name,
        'super'      => undef,
        'subclasses' => hash(),
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
#   ∀ MetaClass A, B | A.clsSuper = B ↔ A ∈ B.clsSubClasses
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
        # if super is being changed ...
        if %self<super> {
            # we need to remove the invocant 
            # from the super's list of subclasses
            %self<super>._removeSubClass($inv);
        }
        # ... and now we can set super    
        %self<super> = $super;
        # now add the invocant to the super's subclasses
        $super.clsSubClasses($inv);
    }
    return %self<super>;
}

sub _removeSubClass(Str $inv: Str $subclass) returns Void {
    get_instance($inv, "Perl::MetaClass")<subclasses>.delete($subclass);
}

sub clsSubClasses(Str $inv: Array *@subclasses) returns Array {
    my %self := get_instance($inv, "Perl::MetaClass");
    if @subclasses {
        my %inv_subclasses = %self<subclasses>;
        # NOTE:
        # enforce the following rules on all @subclasses:
        # - the subclass is an instance of Perl::MetaClass
        # - if the subclass has a superclass, it's superclass is our invocant
        # - if the invocant has a superclass, the subclass is not the superclass of our invocant
        for @subclasses -> $subclass {
            ($subclass.instance_isa('Perl::MetaClass'))
                || die "Sub class must be a Perl::MetaClass instance (got: '$subclass')"; 
            ($subclass.clsSuper() && $subclass.clsSuper().clsName() eq $inv.clsName())
                || die "Sub class's superclass must be the invocant (got: '{ $subclass.clsSuper() }')";    
            ($subclass.clsName() ne $inv.clsSuper().clsName())
                || die "Subclass cannot be the superclass of the invocant"
                    if $inv.clsSuper();
            %inv_subclasses{$subclass} = undef;
        } 
        %self<subclasses> = \%inv_subclasses;    
    }
    return keys(%self<subclasses>);
}

sub clsIsa (Str $inv: Str $class) returns Bool {
    # recurse if we are given a $class instance and not a Class name
    return $inv.clsIsa($class.clsName()) if $class.instance_isa('Perl::MetaClass');
    # if the class name itself matches the class return true
    return 1 if $inv.clsName() eq $class;
    # return false if the $inv has no superclass
    return 0 unless $inv.clsSuper();
    # if the superclass is equal to class then return true
    return 1 if $inv.clsSuper().clsName() eq $class;
    # if not, check against the superclass 
    return $inv.clsSuper().clsIsa($class);
}

# visibility is not as important on the MetaModel as it is on the real
# Class Model.
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

sub clsMethods(Str $inv: Array *@methods) returns Hash {
    my %self := get_instance($inv, "Perl::MetaClass");
    if @methods {
        for @methods -> $key, $value {
            ($value.instance_isa('Perl::MetaMethod'))
                || die "Method values must be a Perl::MetaMethod instance (got: '$value')";             
            my %methods = %self<methods>;
            %methods{$key} = $value;
            %self<methods> = \%methods;                 
        }
    }
    return %self<methods>;
}

sub clsAssocs(Str $inv: Array *@assocs) returns Hash {
    my %self := get_instance($inv, "Perl::MetaClass");
    if @assocs {
        for @assocs -> $key, $value {
            ($value.instance_isa('Perl::MetaAssoc'))
                || die "Assoc values must be a Perl::MetaAssoc instance (got: '$value')";             
            my %assocs = %self<assocs>;
            %assocs{$key} = $value;
            %self<assocs> = \%assocs;                 
        }
    }
    return %self<assocs>;
}


=pod

=head1 NAME

Perl::MetaClass - A meta-model for Perl Classes

=head1 SYNOPSIS

  #         Package
  #          |
  #    +-----+----+
  #    |          |
  #  Module      Role
  #               |
  #             Class
  
  my $package = Perl::MetaClass::new('Package');
  my $role = Perl::MetaClass::new('Role');  
  my $module = Perl::MetaClass::new('Module');    
  
  $role.clsSuperClass($package);
  $module.clsSuperClass($package);  
  
  my $class = Perl::MetaClass::new('Class');  
  $class.clsSuperClass($role);  

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

=item B<clsIsa ($inv: $class)>

This accepts both a Class name and C<$class> instance.

=item B<clsProperties($inv: ?%properties)>

=item B<clsMethods($inv: ?%methods)>

=item B<clsAssocs($inv: ?%assocs)>

=back

=head1 AUTHORS

Sam Vilain

Stevan Little E<lt>stevan@iinteractive.comE<gt>

=cut
