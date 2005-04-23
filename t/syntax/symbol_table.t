#!/usr/bin/pugs

use v6;
require Test;

plan 28;

=kwid

Namespaces, symbol tables and symbolic references.

=cut


my $symhash   = eval '%::';
fail("%:: parse", :todo(1));

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

is(eval '$::("MY::$lex_s")',         $lex,    "loopup of lexical in current scope", :todo(1));
is(eval '$::($global_s)',            $global, "lookup of global in default package", :todo(1));
is(eval '$::("*Main::$global_s")',   $global, "lookup of global in *Main package", :todo(1));
is(eval '~ @::($ary_s)',             ~ @ary,  "array lookup", :todo(1));
is(eval '~ %::($hash_s)',            ~ %hash, "hash lookup", :todo(1));
is(eval '&::($code_s).()',           code(),  "named sub lookup", :todo(1));

ok(eval '!defined($::("nosuch"))',            "unknown scalar lookup", :todo(1));
ok(eval '!defined($::("nosuch"))',            "unknown scalar lookup doesn't autovivify", :todo(1));

ok(eval '!defined($::("MY::nosuch"))',        "unknown lexical lookup", :todo(1));
ok(eval '!defined($::("MY::nosuch"))',        "unknown lexical lookup doesn't autovivify", :todo(1));

# (lvalue lexicals -- see below.)


# symtable hash syntax

is(eval '%MY::{\'$\' ~ $lex_s}',       "bar",   "loopup of lexical in current scope - symtable", :todo(1));
is(eval '%::{\'$\' ~ $global_s}',      $global, "lookup of global in default package - symtable", :todo(1));
is(eval '%*Main::{\'$\' ~ $global_s}', $global, "lookup of global in *Main package - symtable", :todo(1));
is(eval '~ %::{\'@\' ~ $ary_s}',       ~ @ary,  "array lookup - symtable", :todo(1));
is(eval '~ %::{\'%\' ~ $hash_s}',      ~ %hash, "array lookup - symtable", :todo(1));
is(eval '%::{\'&\' ~ $code_s}.()',     code(),  "named sub lookup - symtable", :todo(1));

ok(eval '!defined(%::<nosuch>)',                "unknown scalar lookup", :todo(1));
ok(eval '!defined(%::<nosuch>)',                "unknown scalar lookup doesn't autovivify", :todo(1));

ok(eval '!defined(%MY::<nosuch>)',              "unknown lexical lookup", :todo(1));
ok(eval '!defined(%MY::<nosuch>)',              "unknown lexical lookup doesn't autovivify", :todo(1));

{
	fail("package keyword", :todo(1));
	eval 'package Other1;';

	ok(eval '%:: eq $symhash',       "package declaration changes current package", :todo(1));

	our $new_global =         "It is I.";

	my $lex = "carrot";       # hiding "bar".
	is(eval '$::("MY::$lex_s")',     "carrot",  "loopup of hiding lexical", :todo(1));

	ok(eval '!defined(%::(\'$\' ~ $global_s))', "lookup of global in wrong package", :todo(1)); # XXX: error? warning? silent?
	my $a = eval '$::($global_s)';
	my $b = eval '$::("*Main::$global_s")';
    
	ok (defined $a && defined $b && $a eq $b,   "package search", :todo(1));
	
}

ok(defined eval '%::' && eval '%::' eq $symhash, "previous package declaration was scoped", :todo(1));
is(eval '%::<Other1::$new_global>',  "It is I.", "Global in other package still visible", :todo(1));

