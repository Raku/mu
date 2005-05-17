
use v6;

class Perl::Meta::MetaClass-0.0.1;

use Set;

use Perl::Meta::Property;

sub Perl::Meta::MetaClass::new (?$name) returns Perl::Meta::MetaClass is export {
    return Perl::Meta::MetaClass.new(
        name       => $name,
        subclasses => set(),      
    );
}

has $.name is rw;
has $:parent;
has Set $:subclasses;
has %:properties;
has %:methods;

submethod BUILD($:name, $:subclasses) {}

method isA ($self: Perl::Meta::MetaClass $class) returns Bool {
    # if the class name itself matches the class return true
    return 1 if $self.name() eq $class.name();
    # now go up the hierarchy ...
    my $super = $self.superclass();
    # return false if the $inv has no superclass
    return 0 unless $super.defined;
    # if the superclass is equal to class then return true
    return 1 if $super.name() eq $class.name(); 
    # if not, check against the superclass 
    return $super.isA($class);
} 

method isATypeOf ($self: Str $type) returns Bool {
    return 1 if $self.name eq $type;
    # now go up the hierarchy ...
    my $super = $self.superclass();
    # return false if the $inv has no superclass
    return 0 unless $super.defined;
    # if the superclass is equal to class then return true
    return 1 if $super.name() eq $type; 
    # if not, check against the superclass 
    return $super.isATypeOf($type);    
}

## Superclass methods

method superclass ($self: Perl::Meta::MetaClass ?$super) returns Perl::Meta::MetaClass {
    if $super.defined {   
        # NOTE:
        # we enforce the following rule on superclasses:
        # - the intended super class cannot itself inherit 
        #   from the invocant (circular inheritance)    
        (!$super.isA($self))
            || die "The super class cannot inherit from the invocant (circular inheritance)";            
        # if the parent is defined, then we 
        # are actually changing it, which means
        if $:parent.defined {
            # we need to remove the invocant 
            # from the super's list of subclasses        
            $:parent.:removeSubclass($self);
        }
        # ... and now we can set super          
        $:parent = $super;
        # now add the invocant to the super's subclasses        
        $super.:addSubclass($self);        
    }
    return $:parent;    
}

method allSuperclasses ($self:) returns Array of Perl::Meta::MetaClass {
    # NOTE:
    # I considered making this a Set, however it occured to me that
    # not only is a Set unordered (at least our implementation is)
    # but that it would remove elements from the heirarchy at arbitrary
    # points (because of duplicates) and that is not good
    return ($:parent, $:parent.allSuperclasses()) if $:parent.defined;
}

## Subclass methods

method :removeSubclass ($self: Perl::Meta::MetaClass $subclass) returns Void {
    $:subclasses.remove($subclass);
}

method :addSubclass ($self: Perl::Meta::MetaClass $subclass) returns Void { 
    ($subclass.superclass() && $subclass.superclass().name() eq $self.name())
        || die "Sub class's superclass must be the invocant (got: '{ $subclass.clsSuper() }')";                          
    $:subclasses.insert($subclass);        
}

method subclasses ($self:) returns Array of Perl::Meta::MetaClass {
    $:subclasses.members();
}

method allSubclasses ($self:) returns Array of Perl::Meta::MetaClass {
    # NOTE:
    # again, this is not a Set for the same reasons that allSuperclasses 
    # is not a set (see that method for more info)
    my @all_subclasses;
    for $self.subclasses() -> $subclass {
        @all_subclasses.push($subclass, $subclass.allSubclasses());
    }
    return @all_subclasses;
}

## Properties

method addProperty ($self: Str $label, Perl::Meta::Property $prop) returns Void {
    %:properties{$label} = $prop;
    $prop.associatedWith($self);
}

method removeProperty ($self: Str $label) returns Perl::Meta::Property {
    unless %:properties.exists($label) {
        die "Property '$label' does not exists in this instance";
    }
    my $removed_prop = %:properties{$label};
    $removed_prop.removeAssociation();    
    %:properties.delete($label);
    return $removed_prop;
}

method properties     ($self:) returns Hash  { %:properties        }
method propertyLabels ($self:) returns Array { %:properties.keys() }

=pod

=head1 NAME

Perl::Meta::MetaClass - A meta-meta-model for Perl Classes

=head1 SYNOPSIS

  #         Package
  #          |
  #    +-----+----+
  #    |          |
  #  Module      Role
  #               |
  #             Class
  
  my $package = Perl::Meta::MetaClass.new('Package');
  my $role = Perl::Meta::MetaClass.new('Role');  
  my $module = Perl::Meta::MetaClass.new('Module');    
  
  $role.superclass($package);
  $module.superclass($package);  
  
  my $class = Perl::Meta::MetaClass.new('Class');  
  $class.superclass($role);  
  
  $class.isA('Package');

=head1 DESCRIPTION

Perl::Meta::MetaClass is the meta-model of the Perl6 object system. The code 
in this module itself is the meta-meta-model of the Perl6 object system.

=head1 PUBLIC ATTRIBUTES

=over 4

=item B<$.name is rw>

=back

=head1 METHODS

=over 4

=item B<isA ($self: Perl::Meta::MetaClass $class) returns Bool>

=item B<isATypeOf ($self: Str $type) returns Bool>

=back

=head2 Super/Sub Class methods

=over 4

=item B<superclass ($self: Perl::Meta::MetaClass ?$super) returns Perl::Meta::MetaClass>

=item B<allSuperclasses ($self:) returns Array of Perl::Meta::MetaClass>

=item B<subclasses ($self:) returns Arrary of Perl::Meta::MetaClass>

=item B<allSubclasses ($self:) returns Arrary of Perl::Meta::MetaClass>

=back

=head2 Property methods

=over 4

=item B<addProperty ($self: Str $label, Perl::Meta::Property $prop) returns Void>

=item B<removeProperty ($self: Str $label) returns Perl::Meta::Property>

=back

=head1 SEE ALSO

See the L<Perl::MetaModel> module for more details.

See the F<docs/meta_meta_classes.pod> file as well.

=head1 AUTHORS

Sam Vilain

Stevan Little E<lt>stevan@iinteractive.comE<gt>

=cut
