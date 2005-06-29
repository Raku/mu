
class Role--Perl6 does NameSpace--Perl6;

# publicly exported methods
has Code--Perl6 %.methods;

# private use methods
has Code--Perl6 %:methods;

method new(Str $name) returns Class--Perl6 {
    $.name = $name;
    return $?SELF;
}

method addMethod(Method--Perl6 $meth) {
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

This object represents a Role.

A Role is a composable unit of behaviour; which includes;

=over

=item *

unbound public and private methods, including the signatures of these
methods

=item *

unbound public attributes, which are actually just accessor methods
for variables.

=item *

unbound private attributes, which are actually just accessor methods
as well.

=back

Note that methods are bound into the compiled Class (the Type object)
when they are included into the Class.  This has the same effect as
them being defined in the Class, so this effect is called the
"Flattening" of Roles.

=cut

