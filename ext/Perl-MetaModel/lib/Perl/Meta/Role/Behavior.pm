
use v6;

class Perl::Meta::Role::Behavior;

use Perl::Meta::Property;
use Perl::Meta::Method;

has $:name;
has @:roles;
has %:properties;
has %:methods;

submethod BUILD($:name) {}

method name ($self: Str ?$name) returns Str {
    if $name.defined {
        $:name = $name;
    }
    return $:name;
}

## Roles

method roles ($self:) returns Array {
    @:roles;
}

method addRole ($self: Perl::Meta::Role::Behavior $role) returns Void {
    @:roles.push($role);
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
    for %:properties.kv() -> $label, $prop {
        %props{$label} = $prop;    
    }
    return %props;
}

method isPropertySupported ($self: Str $label) returns Bool {
    return ?$self.getProperty($label); 
}

method getProperty ($self: Str $label) returns Perl::Meta::Preoperty {
    return %:properties{$label} if %:properties.exists($label);
    # Roles have no parents so they do not search "up"
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
    # Roles have no parents so they cannot traverse up ...
    return undef;
}

method isMethodSupported ($self: Str $label) returns Bool {
    $self.findMethod($label) ?? 1 :: 0;
}

=pod

=head1 NAME

Perl::Meta::Role::Behavior - A meta-meta-model for Perl Classes

=head1 SYNOPSIS

=head1 DESCRIPTION

=head1 PUBLIC ATTRIBUTES

=over 4

=item B<$.name is rw>

=back

=head1 METHODS

=head2 Role methods

=over 4

=item B<roles ($self:) returns Array>

=item B<addRole ($self: Perl::Meta::Role::Behavior $role) returns Void>

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

=back

=head1 SEE ALSO

=head1 AUTHORS

Stevan Little E<lt>stevan@iinteractive.comE<gt>

=cut
