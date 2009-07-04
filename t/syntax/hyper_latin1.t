use v6;

use Test;

plan 5;

# Unicode-version of >>~<< only works in utf8 encoded files
#   * Almost all other files in the Pugs Subversion repository are utf-8
#     encoded.
#   * Some editors/editor configurations will always save files as utf-8.
#   * My add-the-usual-svn-properties script sets svn:mime-type to utf-8.
# Because of these reasons, I felt it'd be better to simply encode the latin1
# "<<"s and ">>"s using the \c... notation.

# According to lwall, failure is the desired behaviour.  To use latin1
# in source code, explicitly declare it as such with the `encoding` pragma.

my $latin1_encoded = "(<a b c>) \c187~\c171 (1,2,3)";
my $ASCII_encoded  = "(<a b c>)    >>~<<    (1,2,3)";
my $utf8_encoded   = "(<a b c>)     »~«     (1,2,3)";

#?rakudo todo 'latin1'
eval_dies_ok  $latin1_encoded, "Don't implicitly decode latin1";
eval_lives_ok $ASCII_encoded,  "ASCII as utf8 and works";
eval_lives_ok $utf8_encoded,   "utf8 works";

my $result = 'a1 b2 c3';

is eval( $ASCII_encoded ), $result, 'ASCII gets the right answer';
is eval( $utf8_encoded  ), $result, 'utf8 gets the right answer';
