
use v6;
module Perl::MetaAssoc-0.0.1;

sub Perl::MetaAssoc::new($class) returns Str is export {
    my $id = make_instance("Perl::MetaAssoc", { class => $name });
    return $id;
}

# these could be a little less cut 'n' paste, but for now this is fine
sub assocClass(Str $inv) returns Str is rw {
    return get_instance($inv)<class>;
}

sub assocPair(Str $inv) returns Str is rw {
    return get_instance($inv)<pair>;
}

sub assocRange(Str $inv) returns (Num, Num) is rw {
    return get_instance($inv)<range> ||= [0, inf];
}

sub assocIsComposite(Str $inv) returns Bool is rw {
    return get_instance($inv)<isComposite> ||= ( 0 == 1 );
}

sub assocOrdered(Str $inv) returns Bool is rw {
    return get_instance($inv)<ordered> ||= ( 0 == 1 );
}

sub assocKeyed(Str $inv) returns Bool is rw {
    return get_instance($inv)<keyed> ||= ( 0 == 1 );
}

sub assocCompanion(Str $inc) returns Str is rw {
    return get_instance($inv)<companion>;
}


=pod

=head1 NAME

Perl::MetaAssoc - A meta-model for Perl Classes

=head1 SYNOPSIS

=head1 DESCRIPTION

=head1 AUTHORS

Sam Vilain

Stevan Little E<lt>stevan@iinteractive.comE<gt>

=cut

