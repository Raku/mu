#!/usr/bin/pugs

use v6;
use Test;

plan 1;
# See thread "Packages, Modules and Classes" on p6l
# started by Stevan Little:
# L<"http://www.nntp.perl.org/group/perl.perl6.language/23019">
skip_rest "test needs to be rewritten because of recent design changes";

=begin obsolete

plan 28;

=kwid

Namespaces, symbol tables and symbolic references.

#=cut

# flunk("%:: parse", :todo);
eval_ok('%::', '%:: parses', :todo);
my $symhash   = eval '%::';

my  $lex      = 'bar';
our $lex_s    = 'lex';
our $global   = 'buz';     # global *scalar*, that is; unlike $lex.
our $global_s = 'global';
our @ary      = <a b c d>;
our $ary_s    = 'ary';
#our %hash     = (<1 2 3 4>);
our %hash     = (1 => 2, 3 => 4);
our $hash_s   = 'hash';
sub code        { 42 }
our $code_s   = 'code';

# symref syntax

is try {$::("MY::$lex_s")},       $lex,    "loopup of lexical in current scope", :todo;
is try {$::($global_s)},          $global, "lookup of global in default package";
is try {$::("*Main::$global_s")}, $global, "lookup of global in *Main package", :todo;
is try {~ @::($ary_s)},           ~ @ary,  "array lookup";
is try {~ %::($hash_s)},          ~ %hash, "hash lookup";
is try {&::($code_s).()},         code(),  "named sub lookup";

ok(try { !defined($::("nosuch")) },     "unknown scalar lookup", :todo);
ok(try { !defined($::("nosuch")) },     "unknown scalar lookup doesn't autovivify", :todo);

ok(try { !defined($::("MY::nosuch")) }, "unknown lexical lookup");
ok(try { !defined($::("MY::nosuch")) }, "unknown lexical lookup doesn't autovivify");

# (lvalue lexicals -- see below.)


# symtable hash syntax

is(try {%MY::{'$' ~ $lex_s}},       "bar",   "loopup of lexical in current scope - symtable", :todo);
is(eval('%::{\'$\' ~ $global_s}'),  $global, "lookup of global in default package - symtable", :todo);
is(try {%*Main::{'$' ~ $global_s}}, $global, "lookup of global in *Main package - symtable", :todo);
is(eval('~ %::{\'@\' ~ $ary_s}'),       ~ @ary,  "array lookup - symtable", :todo);
is(eval('~ %::{\'%\' ~ $hash_s}'),      ~ %hash, "array lookup - symtable", :todo);
is(eval('%::{\'&\' ~ $code_s}.()'),     code(),  "named sub lookup - symtable", :todo);

ok(eval('!defined(%::<nosuch>)'),                "unknown scalar lookup", :todo);
ok(eval('!defined(%::<nosuch>)'),                "unknown scalar lookup doesn't autovivify", :todo);

ok(try {!defined(%MY::<nosuch>)},              "unknown lexical lookup");
ok(try {!defined(%MY::<nosuch>)},              "unknown lexical lookup doesn't autovivify");

{
    # flunk("package keyword", :todo);
    ok eval('package Other1;'), "package keyword parses", :todo;

    ok eval('%:: eq $symhash'), "package declaration changes current package", :todo;

    our $new_global =         "It is I.";

    my $lex = "carrot";       # hiding "bar".
    is(try {$::("MY::$lex_s")},     "carrot",  "loopup of hiding lexical", :todo);

    ok(try {!defined(%::('$' ~ $global_s))}, "lookup of global in wrong package", :todo); # XXX: error? warning? silent?
    my $a = try {$::($global_s)};
    my $b = try {$::("*Main::$global_s")};

    ok(defined $a && defined $b && $a eq $b,   "package search", :todo);

}

ok(defined eval('%::') && eval('%::') eq $symhash, "previous package declaration was scoped", :todo);
is(eval('%::<Other1::$new_global>'),  "It is I.", "Global in other package still visible", :todo);
