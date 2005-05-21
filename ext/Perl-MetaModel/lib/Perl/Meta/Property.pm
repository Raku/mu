
use v6;

class Perl::Meta::Property-0.0.1;

has Perl::Meta::Type  $:type;
has Any               $:default;
has Str               $:trait;
has Perl::Meta::Class $:associated_with;

submethod BUILD (Perl::Meta::Type $:type, $:default, $:trait) {}

method type ($self: Perl::Meta::Type ?$type) returns Perl::Meta::Type {
    if $type.defined {
        if $:type.name() ne $type.name() {
            $:default = undef;
        }
        $:type = $type;
    }
    return $:type;
}

method default ($self: Any ?$value) returns Any {
    if $value.defined {
        die "Incorrect value type for property default"
            unless $value.isa($:type.name()) ||
                   $value.isa('Perl::Meta::Class') && $value.isATypeOf($:type.name());
        $:default = $value;
    }
    return $:default;
}

method trait ($self: Str ?$trait) returns Str {
    if $trait.defined {
        ($trait ~~ rx:perl5/^r[ow]$/)
            || die "Currently only 'ro' and 'rw' traits are supported";
        $:trait = $trait;
    }
    return $:trait;
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

Stevan Little E<lt>stevan@iinteractive.comE<gt>

=cut
