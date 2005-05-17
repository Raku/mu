
use v6;

class Perl::Meta::Property-0.0.1;

has $:type;
has $:default;
has $:associated_with;

submethod BUILD ($:type, $:default) {}

method type ($self: Str ?$type) returns Str {
    if $type.defined {
        if $:type ne $type {
            $:default = undef;
        }
        $:type = $type;
    }
    return $:type;
}

method default ($self: Any ?$value) returns Any {
    if $value.defined {
        die "Incorrect value type for property default"
            unless $value.isa($:type) ||
                   $value.isa('Perl::Meta::Class') && $value.isATypeOf($:type);
        $:default = $value;
    }
    return $:default;
}

method associatedWith ($self: Perl::Meta::Class ?$class) returns Perl::Meta::Class {
    if $class.defined {
        (!$:associated_with.defined)
            || die "This property has already be associated with a something";            
        $:associated_with = $class;
    }
    return $:associated_with;
}

method removeAssociation ($self:) returns Void {
    $:associated_with = undef;
}

=pod

=head1 NAME

Perl::Meta::Property

=head1 SYNOPSIS

  use Perl::Meta::Property;

=head1 DESCRIPTION

=head1 METHODS

=over 4

=item B<type ($self: Str ?$type) returns Str>

=item B<default ($self: Any ?$value) returns Any>

=item B<associatedWith ($self: Perl::Meta::Class ?$class) returns Perl::Meta::Class>

=item B<removeAssociation ($self:) returns Void>

=back

=head1 AUTHORS

Sam Vilain

Stevan Little E<lt>stevan@iinteractive.comE<gt>

=cut
