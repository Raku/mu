
use v6;
module Perl::MetaProperty-0.0.1;

use Hack::Instances; 

sub Perl::MetaProperty::new(Str $type, Any +$default) returns Str is export {
    my $id = make_instance("Perl::MetaProperty", { 
        'type'    => $type,
        'default' => undef
    });
    $id.propDefault($default) if $default.defined;
    return $id;
}

sub propType(Str $inv: Str ?$type) returns Str {
    my %self := get_instance($inv, "Perl::MetaProperty");
    if $type.defined {
        # NOTE:
        # if the type is changed, then the default value
        # is invalidated since it the default and type
        # may not match anymore. Not doing this would
        # destabilize the model
        if %self<type> ne $type {
            %self<default> = undef;
        }
        %self<type> = $type;         
    }
    return %self<type>;
}

sub propDefault(Str $inv: Any ?$default) returns Any {
    my %self := get_instance($inv, "Perl::MetaProperty");
    if $default.defined {
        # NOTE:
        # we check regular .isa() to account for 
        # builtin types, and we check our Hack::Instance
        # .instance_isa() and .clsName() as well. 
        # XXX - We should also have something which can 
        # climb the class isa() tree as well.
        my $prop_type = $inv.propType();
        die "Incorrect Type value for property default (got: '{ $default } -> { ref($default) }', expected: '$prop_type')"
            unless $default.isa($prop_type) || 
                        ($default.instance_isa('Perl::MetaClass') && $default.clsName() eq $prop_type);
        %self<default> = $default;
    }
    return %self<default>;
}

=pod

=head1 NAME

Perl::MetaProperty - A meta-model for Perl Classes

=head1 SYNOPSIS

  use Perl::MetaProperty;

=head1 DESCRIPTION

=head1 FUNCTIONS

=over 4

=item B<Perl::MetaProperty::new(Str $type, Any ?$default)>

=item B<propType($inv: ?$type)>

=item B<propDefault($inv: ?$default)>

=back

=head1 AUTHORS

Sam Vilain

Stevan Little E<lt>stevan@iinteractive.comE<gt>

=cut

