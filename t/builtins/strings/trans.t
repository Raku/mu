#!/usr/bin/pugs

use v6;
use Test;

=kwid

String transliteration

=cut


plan 7;

is("ABC".trans( 'A'=>'a', 'B'=>'b', 'C'=>'c' ),"abc",
        "Each side can be individual characters");

is("XYZ".trans( 'XYZ' => 'xyz' ),"xyz",
           "The two sides of the any pair can be strings interpreted as tr/// would multichar");


is("ABC".trans( 'A-C' => 'a-c' ),"abc",
           "The two sides of the any pair can be strings interpreted as tr/// would range");

#skip 2, "loops forever with Pugs 6.2.10";
is("ABCDEF".trans( 'AB-E' => 'ab-e' ), "abcdeF",
                  "The two sides can consists of both chars and ranges");
is("ABCDEFGH".trans( 'A-CE-G' => 'a-ce-g' ),"abcDefgH",
                  "The two sides can consist of multiple ranges");

# These will need the way the hashses deal with pairs.

# This works by accedent.
is("ABCXYZ".trans( ['A'..'C'] => ['a'..'c'], <X Y Z> => <x y z> ),"abcxyz",
           "The two sides of each pair may also be array references" );

is("ABCDE".trans( ['A' .. 'E'] => "a-e" ), "abcde",
	   "Using array reference on one side and string range on the other" );

is(" <>&".trans( [' ',      '<',    '>',    '&'    ] => 
                 ['&nbsp;', '&lt;', '&gt;', '&amp;' ]),"&nbsp;&lt;&gt;&amp;",
         "The array version can map one-or-more characters to one-or-more characters"
         ,:todo);
