
use v6;
module Hack::Instances-0.0.1;

my %INSTANCES;

sub make_instance($class, $obj is rw) returns Str is export {
    my $id;
    while ( !$id.defined or %INSTANCES.exists($id) ) {
	    $id = "OBJECT;$class;" ~ substr(rand() ~ "", 2, 15);
    }
    %INSTANCES{$id} = $obj;
    return $id;
}

sub get_instance(Str $inst) is export {
    die "The instance '$inst' is not a valid instance (key not found)" 
        unless %INSTANCES.exists($inst);
    my $self := %INSTANCES{$inst};
    return $self;
}

=pod

=head1 NAME

Hack::Instances - An abstraction of inside-out classes

=head1 SYNOPSIS

  use Hack::Instance;

  sub My::Class::new returns Str is export {
	  my %self = ( "value" => 3 );
	  make_instance("My::Class", %self);
  }

  sub counter(Str $inv:) returns Int {
      my $self = get_instance($inv);
      my $value = ++$self<value>;
      return $value;
  }
  
  my $object1 = My::Class::new();
  my $object2 = My::Class::new();

  say $object1.counter(); # prints 4
  say $object2.counter(); # prints 4
  say $object1.counter(); # prints 5

=head1 DESCRIPTION

This is a basic module to help with the creation of inside-out objects 
in Pugs.

=head1 LIMITATIONS & CAVEATS

Currently the functionality this module provides is fairly fragile. 
Since we do not "create" a new type when we create a class/instance, there
is nothing directly associating a set of methods with a particular 
namespace. Methods are essentially just functions, whose first parameter
is the instance key, and who are written to use the invocant notation.

This means that methods of the same name will get "clobbered" and methods 
from one "class" can be called by instances of another "class".

But all this said, it is still possible to build a working "object" on 
some level.

=head1 FUNCTIONS

=over 4

=item B<make_instance ($class, $obj is rw) returns Str>

This will create an inside-out instance of C<$class> with the C<$obj> given.
The C<$obj> is usually a hash value, but can be anything which fits into a 
Scalar. 

This function then returns a string which is the key into our global 
C<%INSTNACES> hash. The string constructed is a unique identifier for our
object.

=item B<get_instance (Str $inst)>

Given the string key C<$inst>, this function will return the instance stored in
our global C<%INSTANCES> hash.

If the Str C<$inst> is not found in our global C<%INSTANCES> hash, an exception
is thrown.

=back

=head1 AUTHORS

Sam Vilain

Stevan Little E<lt>stevan@iinteractive.comE<gt>

=cut
