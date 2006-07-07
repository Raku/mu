use v6-alpha;

use Test;
plan 1;

# See thread "Packages, Modules and Classes" on p6l
# started by Stevan Little:
# L<"http://www.nntp.perl.org/group/perl.perl6.language/23019">
skip_rest "test needs to be rewritten because of recent design changes";
exit;

=begin obsolete

plan 15;

# Reflecting on symbol tables. Let's start with the 'default/main' symbol table.
# I think it's called %*::, but I can easily be proven wrong.

our $scalar = 42;
our @array = (1..13);
our %hash = ( :car<cdr> ); # Sue me, I quite like lisp.

ok( %*::.exists('$scalar'), 'Default namespace %*:: is populated with scalars',
       :todo<feature> );
is(%*::{'$scalar'}, 42, 'Symbolic lookup of $scalar', :todo<feature>);

ok( %*::.exists('@array'), 'Default namespace %*:: is populated with arrays',
       :todo<feature>);
is(%*::{'@array'}[0], 1, 'symbolic lookup of @array', :todo<feature>);

ok( %*::.exists('%hash'), 'Default namespace %*:: is populated with arrays',
       :todo<feature>);
is(%*::{'%hash'}<car>, 'cdr', 'symbolic lookup of %hash', :todo<feature>);

# Now let's check %OUR::

ok( %OUR::.exists('$scalar'),
    'Pseudo namespace %OUR:: is populated with scalars',
   :todo<feature> );
is(%OUR::{'$scalar'}, 42, 'Symbolic lookup of $OUR::scalar', :todo<feature>);

ok( %OUR::.exists('@array'), 'Pseudo namespace %OUR:: is populated with arrays',
       :todo<feature>);
is(%OUR::{'@array'}[0], 1, 'symbolic lookup of @OUR::array', :todo<feature>);

ok( %OUR::.exists('%hash'), 'Pseudo namespace %OUR:: is populated with arrays',
       :todo<feature>);
is(%*::{'%hash'}<car>, 'cdr', 'symbolic lookup of %OUR::hash', :todo<feature>);

# So the easy stuff's out of the way, let's introduce a package.

package Symbols::R::Us {
    our $scalar = 21;
    our @array  = (14..26);
    our %hash   = ( :key<value> );
}

ok( %*::.exists('%Symbols::R::Us::'),
    "A namespace is just another hash variable",
    :todo<feature>);

is( %Symbols::R::Us::{'$scalar'}, 21, "Symbolic lookup of a package variable",
    :todo<feature>);
is( %*::{'%Symbols::R::Us::'}{'@array'}[0], 14, "Double indirect lookup",
    :todo<feature>);
