
use v6;
module Perl::MetaClass-0.0.1;

BEGIN {
    make_class("Perl::MetaClass");
}

sub Perl::MetaClass::new($name) returns Perl::MetaClass is export {
    my $id = make_instance("Perl::MetaClass", { name => $name });
    return $id;
}

# these could be a little less cut 'n' paste, but for now this is fine
sub clsName(Perl::MetaClass $inv) returns Str is rw {
    return get_instance($inv)<name>;
}

# FIXME - the below methods do not enforce the rule;
#  ∃ MetaClass A, B | A.clsSuper = B ↔ A ∈ B.clsSubClasses
sub clsSuper(Perl::MetaClass $inv) returns Perl::MetaClass is rw {
    return get_instance($inv)<super>;
}

sub clsSubClasses(Perl::MetaClass $inv) returns Array(Perl::MetaClass) is rw {
    return get_instance($inv)<subclasses> ||= [];
}

sub clsProperties(Perl::MetaClass $inv) returns Map(Str, Perl::MetaProperty) is rw {
    return get_instance($inv)<subclasses> ||= {};
}

sub clsMethods(Perl::MetaClass $inv) returns Map(Str, Perl::MetaMethod) is rw {
    return get_instance($inv)<methods> ||= {};
}

sub clsAssocs(Perl::MetaClass $inc) returns Map(Str, Perl::MetaAssoc) is rw {
    return get_instance($inv)<assocs> ||= {};
}


=pod

=head1 NAME

Perl::MetaClass - A meta-model for Perl Classes

=head1 SYNOPSIS

 my $class_mc = Perl::MetaClass->new("Class");
 my $role_mc = Perl::MetaClass->new("Role");

=head1 DESCRIPTION

A Perl::MetaClass object is an object which holds objects that
describe the Perl 6 Class system (or, potentially, any other Class
system too).

=head1 PRIOR ART

In the T2 CPAN module, the Class::Tangram module is behaving as a
Class Meta-Model, and T2 is describing the Class Model.  A set of T2
objects represent a set of Classes.

However, the T2 module only represents a classical single inheritance
model without interfaces, so cannot represent everything that the
Roles-based model of Perl 6 will.

=head1 AUTHORS

Sam Vilain

Stevan Little E<lt>stevan@iinteractive.comE<gt>

=cut
