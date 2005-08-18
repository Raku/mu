#!/usr/bin/pugs
use v6;
use Test;

=kwid

=head1 indirect object notation call tests

These tests are the testing for "Method" section of Synopsis 12

L<<S12/"Method" /Indirect object notation now requires a colon after the invocant if there are any arguments.*the same thing:/>>

=cut

plan 7;


##### Without arguments
class T1
{
	method a
	{
		'test';
	}
}

my T1 $o .= new;
ok( "Still alive after new" );

is( $o.a(), 'test', "The indirect object notation call without argument 1" );
is( (a $o:), 'test', "The indirect object notation call without arguments 2" );
is( (a $o), 'test', "The indirect object notation call without arguments 3" );

##### With arguments
class T2
{
	method a( $x )
	{
		$x;
	}
}

my T2 $o .= new;
ok( "Still alive after new" );
my $seed = rand(1000);
is( $o.a( $seed ), $seed, "The indirect object notation call with argument 1" );
is( (a $o: $seed), $seed, "The indirect object notation call with arguments 2" );

