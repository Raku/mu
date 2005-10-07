
class Type--Perl6;

has Class $.meta;

# this is the compiled dispatch table... accessors should be in here
has Method %.dispatch;

# private accessors, methods, etc
has Method %:dispatch;

# next type(s) for dispatchable lookup (inheritance)
has Type @.supers;

multi method isa($self: Class $class) returns Bool {
    .meta.isa($class);
}

=head1 NAME

Type - Perl 6 types

=head1 SYNOPSIS

 my $class = Class.new("SomeClass");
 my $type = Type.new(Class.new("SomeClass"));

 $class.apply;

 $type =:= ::SomeClass;   # true

=head1 DESCRIPTION

Every variable in Perl 6 has a type.  This type can be accessed using
the C<.ref> method, which is available on any type, boxed or unboxed.

These objects are what you get from C<::SomeClass> references.

Note that this could be a different entity to the objects that are
created when you use;

  type Bar;
  subtype Foo of Str where { rx/^Foo/ };

It is probably bad to have both a MetaClass called C<Type> and a
language object called C<Type>.  Perhaps this module should be called
something else like C<t>, or maybe they are one and the same as what
you get when you use the C<type> keyword.  Or maybe the full package
name (C<Type--Perl6>) disambiguates this case.

Types have a dispatch table; this is a collection of functions and
methods that are queried during dispatch (which includes attribute
lookup as well as method lookup, as attribute fetching is actually
just a shorthand for calling an accessor).

Run-time inheritance/method dispatch need not look at the meta
objects; each dispatch table points directly to its "superclass"
dispatch table(s).

=cut

