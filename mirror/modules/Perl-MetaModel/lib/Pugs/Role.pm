
class Perl::Role-0.01;

isa Perl::Package;

has Array of Perl::Roles @.super_roles;

has Hash of Str, Perl::Meta::Property %.properties;
has Hash of Str, Perl::Method %.methods;
