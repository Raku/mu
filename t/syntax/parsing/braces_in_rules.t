use v6-alpha;


=pod

The parser is confused by rules containing \{ .

A closing \} doesn't help.  A closing <'}'> does.
Given
 rule foo { \{ }
 ... a file full of code ...
 #}'
the existence of the comment determines whether the file will parse.
With this comment, it doesn't.  Remove or modify the comment, it does.

Which suggests, perhaps, that the parser, when attempting to parse such
a rule, is wandering far far afield, and only backtracking to something
reasonable when/if it isn't pursuaded by distant things that it isn't
approaching this all wrong.  But it is approaching it wrong.

=cut

use Test;
plan 1;

# rule foo { \{ }  #}'
is(eval(' rule foo { \{ } #}\'
3 '), 3, 'expression parsed');
