
use v6;
module Perl::MetaProperty-0.0.1;

sub Perl::MetaProperty::new(Type $type) returns Str is export {
    my $id = make_instance("Perl::MetaProperty", { type => $type });
    return $id;
}

sub propType(Str $inv) returns Type is rw {
    return get_instance($inv)<type> ||= [];
}

sub propDefault(Str $inv) is rw {
    return get_instance($inv)<default>;
}


