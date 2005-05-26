
class Role;

has $.name;

method new(Str $name) returns Class {
    $.name = $name;
    return $?SELF;
}

method addMethod(Method $meth) {
    ...
}

method apply() {
    ...
}


=head1 NAME

Role - Entities in a program's model (M1)

=head1 SYNOPSIS

 my $role = Role.new("SomeRole");

 $role.addMethod(Method.new(...))

 $role.apply;

=head1 DESCRIPTION

This object represents a Role.  It is what is created when you
declare a new Role.

=cut

