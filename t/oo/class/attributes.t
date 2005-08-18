#!/usr/bin/pugs
use v6;
use Test;


plan 13;

=kwid

=head1 indirect object notation call tests

These tests are the testing for "Attribute" section of Synopsis 12

L<<S12/"Attributes" /Attributes are stored an an opaque datatype,*use has:/>>

=cut
# has
eval_ok '
class C1 { has $.a; has $:b; }; 
my C1 $o .= new;
ok( "Still alive after new" );
is( $o.a(), undef, "testing for public attribute declared by has" );
dies_ok( {$o.b()}, "testing for private attribute declared by has" );
';
eval_ok '
class C2 { has $.a = 1; has $:b = 2; };
my C2 $o .= new;
ok( "Still alive after new" );
is( $o.a(), 1, "testing the value for public attribute declared by has" );
dies_ok( {$o.b()}, "testing the value for private attribute declared by has" );
';


=kwid

=head1 indirect object notation call tests

These tests are the testing for "Attribute" section of Synopsis 12

L<<S12/"Attributes" /Class attributes are declared with either my or our.*according to the secondary sigil:/>>

=cut
# my
eval_ok '
class C3 { my $.a; my $:b; }; 
my C3 $o .= new;
ok( "Still alive after new" );
is( $o.a(), undef, "testing for public attribute declared by my" );
dies_ok( {$o.b()}, "testing for private attribute declared by my" );
';
eval_ok '
class C4 { my $.a = 1; my $:b = 2; };
my C4 $o .= new;
ok( "Still alive after new" );
is( $o.a(), 1, "testing the value for public attribute declared by my" );
dies_ok( {$o.b()}, "testing the value for private attribute declared by my" );
';

# our
eval_ok '
class C5 { our $.a; our $:b; }; 
my C5 $o .= new;
ok( "Still alive after new" );
is( $o.a(), undef, "testing for public attribute declared by our" );
dies_ok( {$o.b()}, "testing for private attribute declared by our" );
';
eval_ok '
class C6 { our $.a = 1; our $:b = 2; };
my C6 $o .= new;
ok( "Still alive after new" );
is( $o.a(), 1, "testing the value for public attribute declared by our" );
dies_ok( {$o.b()}, "testing the value for private attribute declared by our" );
';
