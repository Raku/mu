
use v6;

class Perl::Meta::Class-0.0.1;

use Set;
use Perl::Meta::Property;
use Perl::Meta::Method;

sub Perl::Meta::Class::new (?$name) returns Perl::Meta::Class is export {
    return Perl::Meta::Class.new(
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

method isA ($self: Perl::Meta::Class $class) returns Bool {
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

method superclass ($self: Perl::Meta::Class ?$super) returns Perl::Meta::Class {
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

method allSuperclasses ($self:) returns Array of Perl::Meta::Class {
    # NOTE:
    # I considered making this a Set, however it occured to me that
    # not only is a Set unordered (at least our implementation is)
    # but that it would remove elements from the heirarchy at arbitrary
    # points (because of duplicates) and that is not good
    return ($:parent, $:parent.allSuperclasses()) if $:parent.defined;
}

## Subclass methods

method :removeSubclass ($self: Perl::Meta::Class $subclass) returns Void {
    $:subclasses.remove($subclass);
}

method :addSubclass ($self: Perl::Meta::Class $subclass) returns Void { 
    ($subclass.superclass() && $subclass.superclass().name() eq $self.name())
        || die "Sub class's superclass must be the invocant (got: '{ $subclass.clsSuper() }')";              
    $:subclasses.insert($subclass);        
}

method subclasses ($self:) returns Array of Perl::Meta::Class {
    $:subclasses.members();
}

method allSubclasses ($self:) returns Array of Perl::Meta::Class {
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

method allProperties ($self:) returns Hash {
    my %props;
    $self.:allProperties(\%props);
    return %props;
}

method :allProperties ($self: Hash %props is rw) returns Void {
    # collect all the parents first ...
    $:parent.:allProperties(\%props) if $:parent.defined;
    # then all the local ones will overwrite ...
    for %:properties.kv() -> $label, $prop {
        %props{$label} = $prop;    
    }
}

method isPropertySupported ($self: Str $label) returns Bool {
    return %:properties{$label} if %:properties.exists($label);
    return $:parent.isPropertySupported($label) if $:parent.defined;
    return undef;    
}

## Methods

method addMethod ($self: Str $label, Perl::Meta::Method $method) returns Void {
    %:methods{$label} = $method;
    $method.associatedWith($self);
}

method removeMethod ($self: Str $label) returns Perl::Meta::Method {
    unless %:methods.exists($label) {
        die "Method '$label' does not exists in this instance";
    }
    my $removed_method = %:methods{$label};
    $removed_method.removeAssociation();    
    %:methods.delete($label);
    return $removed_method;
}

method methods      ($self:) returns Hash  { %:methods        }
method methodLabels ($self:) returns Array { %:methods.keys() }

method findMethod ($self: Str $label) returns Perl::Meta::Method {
    return %:methods{$label} if %:methods.exists($label);
    return $:parent.findMethod($label) if $:parent.defined;
    return undef;
}

method isMethodSupported ($self: Str $label) returns Bool {
    $self.findMethod($label) ?? 1 :: 0;
}

method invokeMethod ($self: Str $label, $inv, *@args) returns Any {  
    my $method = $self.findMethod($label);
    ($method.defined)
        || die "Method not found";
    my $impl = $method.code();
    ($impl.defined)
        || die "Method has no code";    
    return $impl($inv, *@args);
}


=pod

=head1 NAME

Perl::Meta::Class - A meta-meta-model for Perl Classes

=head1 SYNOPSIS

  #         Package
  #          |
  #    +-----+----+
  #    |          |
  #  Module      Role
  #               |
  #             Class
  
  my $package = Perl::Meta::Class.new(name => 'Package');
  my $role = Perl::Meta::Class.new(name => 'Role');  
  my $module = Perl::Meta::Class.new(name => 'Module');    
  
  $role.superclass($package);
  $module.superclass($package);  
  
  my $class = Perl::Meta::Class.new(name => 'Class');  
  $class.superclass($role);  
  
  $class.isA('Package');

=head1 DESCRIPTION

Perl::Meta::Class is the meta-model of the Perl6 object system. The code 
in this module itself is the meta-meta-model of the Perl6 object system.

=head1 PUBLIC ATTRIBUTES

=over 4

=item B<$.name is rw>

=back

=head1 METHODS

=over 4

=item B<isA ($self: Perl::Meta::Class $class) returns Bool>

=item B<isATypeOf ($self: Str $type) returns Bool>

=back

=head2 Super/Sub Class methods

=over 4

=item B<superclass ($self: Perl::Meta::Class ?$super) returns Perl::Meta::Class>

=item B<allSuperclasses ($self:) returns Array of Perl::Meta::Class>

=item B<subclasses ($self:) returns Arrary of Perl::Meta::Class>

=item B<allSubclasses ($self:) returns Arrary of Perl::Meta::Class>

=back

=head2 Property methods

=over 4

=item B<addProperty ($self: Str $label, Perl::Meta::Property $prop) returns Void>

=item B<removeProperty ($self: Str $label) returns Perl::Meta::Property>

=item B<properties ($self:) returns Hash>

=item B<propertyLabels ($self:) returns Array>

=item B<allProperties ($self:) returns Hash>

=item B<isPropertySupported ($self: Str $label) returns Bool>

=back

=head2 Method methods

=over 4

=item B<addMethod ($self: Str $label, Perl::Meta::Method $prop) returns Void>

=item B<removeMethod ($self: Str $label) returns Perl::Meta::Method>

=item B<methods ($self:) returns Hash>

=item B<methodLabels ($self:) returns Array>

=item B<findMethod ($self: Str $label) returns Perl::Meta::Method>

=item B<isMethodSupported ($self: Str $label) returns Bool>

=item B<invokeMethod ($self: Str $label, *@args) returns Any>

=back

=head1 SEE ALSO

See the L<Perl::MetaModel> module for more details.

See the F<docs/meta_meta_classes.pod> file as well.

=head1 AUTHORS

Stevan Little E<lt>stevan@iinteractive.comE<gt>

Sam Vilain

=cut
