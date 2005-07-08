#!/usr/bin/pugs

use v6;
use Test;

=kwid

String transliteration

=cut

is("ABC".trans( 'A'=>'a', 'B'=>'b', 'C'=>'c' ),"abc",
		"Each side can be individual characters",:todo);

is("ABCXYZ".trans( 'A-C' => 'a-c', 'XYZ' => 'xyz' ),"abcxyz",
		   "The two sides of the any pair can be strings interpreted as tr/// would",:todo);

is("ABCXYZ".trans( ['A'..'C'] => ['a'..'c'], <X Y Z> => <x y z> ),"abcxyz",
		   "The two sides of each pair may also be array references"
		   ,:todo);

is(" <>&".trans( [' ',      '<',    '>',    '&'    ] => 
                 ['&nbsp;', '&lt;', '&gt;', '&amp;' ]),"&nbsp;&lt;&gt;&amp;",
		 "The array version can map one-or-more characters to one-or-more characters"
		 ,:todo);
