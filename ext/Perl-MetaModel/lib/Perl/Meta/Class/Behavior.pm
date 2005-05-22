
use v6;
use Perl::Meta::Role::Behavior;

class Perl::Meta::Class::Behavior is ::Perl::Meta::Role::Behavior;

use Set;

has $:parent;
has Set $:subclasses;

method isA ($self: Perl::Meta::Class::Behavior $class) returns Bool {
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

method superclass ($self: Perl::Meta::Class::Behavior ?$super) returns Perl::Meta::Class::Behavior {
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

method allSuperclasses ($self:) returns Array of Perl::Meta::Class::Behavior {
    # NOTE:
    # I considered making this a Set, however it occured to me that
    # not only is a Set unordered (at least our implementation is)
    # but that it would remove elements from the heirarchy at arbitrary
    # points (because of duplicates) and that is not good
    return ($:parent, $:parent.allSuperclasses()) if $:parent.defined;
}

## Subclass methods

method :removeSubclass ($self: Perl::Meta::Class::Behavior $subclass) returns Void {
    $:subclass = set() unless $:subclasses.defined;
    $:subclasses.remove($subclass);
}

method :addSubclass ($self: Perl::Meta::Class::Behavior $subclass) returns Void { 
    $:subclass = set() unless $:subclasses.defined;
    ($subclass.superclass() && $subclass.superclass().name() eq $self.name())
        || die "Sub class's superclass must be the invocant (got: '{ $subclass.clsSuper() }')";              
    $:subclasses.insert($subclass);        
}

method subclasses ($self:) returns Array of Perl::Meta::Class::Behavior {
    $:subclass = set() unless $:subclasses.defined;
    $:subclasses.members();
}

method allSubclasses ($self:) returns Array of Perl::Meta::Class::Behavior {
    $:subclass = set() unless $:subclasses.defined;
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
    my $prop = $self.::Perl::Meta::Role::Behavior::isPropertySupported($label);
    return $prop if $method.defined;
    return $:parent.isPropertySupported($label) if $:parent.defined;
    return undef;    
}

## Methods

method findMethod ($self: Str $label) returns Perl::Meta::Method {
    my $method = $self.::Perl::Meta::Role::Behavior::findMethod($label);
    return $method if $method.defined;
    # otherwise, go to the parent ...
    return $:parent.findMethod($label) if $:parent.defined;
    # and lastly return undef for failure
    return undef;
}

method invokeMethod ($self: Str $label, $inv, *@args) returns Any {  
    my $method = $self.findMethod($label);
    ($method.defined)
        || die "Method not found";
    return $method.invoke($inv, @args);
}

=pod

=head1 NAME

Perl::Meta::Class::Behavior - A meta-meta-model for Perl Classes

=head1 SYNOPSIS

=head1 DESCRIPTION

=head1 SUPERCLASS

=over 4

=item B<Perl::Meta::Role::Behavior>

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

=item B<allProperties ($self:) returns Hash>

=item B<isPropertySupported ($self: Str $label) returns Bool>

=back

=head2 Method methods

=over 4

=item B<findMethod ($self: Str $label) returns Perl::Meta::Method>

=item B<invokeMethod ($self: Str $label, *@args) returns Any>

=back

=head1 SEE ALSO

=head1 AUTHORS

Stevan Little E<lt>stevan@iinteractive.comE<gt>

=cut
