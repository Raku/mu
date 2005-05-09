
use v6;
module Perl::MetaClass-0.0.1;

BEGIN {
    make_class("Perl::MetaClass");
}

sub Perl::MetaClass::new($name) returns Perl::MetaClass is export {
    my $id = make_instance("Perl::MetaClass", { name => $name });
    return $id;
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
