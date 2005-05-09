
use v6;
module Perl::MetaMethod-0.0.1;

sub Perl::MetaMethod::new($sub) returns Str is export {
    my $id = make_instance("Perl::MetaMethod", { sub => $sub });
    return $id;
}

sub methodParams(Str $inv) returns Array is rw {
    return get_instance($inv)<params> ||= [];
}

sub methodInvoke(Str $inv) {
    shift;
    return get_instance($inv)<sub>(@_);
}


