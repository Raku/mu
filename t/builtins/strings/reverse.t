use v6-alpha;

use Test;

=pod

Basic test for the reverse() builtin with a string (Str).

=cut

plan 8;

#as a function :
is( reverse('Pugs'), 'sguP', "as a function");

#as a method :
is( "".reverse, "", "empty string" );
is( 'Hello World !'.reverse, '! dlroW olleH', "literal" );

#on a variable ?
my Str $a = 'Hello World !';
is( $a.reverse, '! dlroW olleH', "with a Str variable" );
is( $a, 'Hello World !', "reverse should not be in-place" );
is( $a .= reverse, '! dlroW olleH', "after a .=reverse" );

#multiple iterations (don't work in 6.2.12) :
is( 'Hello World !'.reverse.reverse, 'Hello World !', 
        "two reverse in a row." );
        
#reverse with unicode :
is( 'ä€»«'.reverse,   '«»€ä', "some unicode characters" );

