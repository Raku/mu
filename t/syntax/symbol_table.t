#!/usr/bin/pugs

use v6;
require Test;

plan 28;

=kwid

Namespaces, symbol tables and symbolic references.

=cut


my $symhash   = eval '%::';
todo_fail("%:: parse");

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

todo_is(eval '$::("MY::$lex_s")',         $lex,    "loopup of lexical in current scope");
todo_is(eval '$::($global_s)',            $global, "lookup of global in default package");
todo_is(eval '$::("*Main::$global_s")',   $global, "lookup of global in *Main package");
todo_is(eval '~ @::($ary_s)',             ~ @ary,  "array lookup");
todo_is(eval '~ %::($hash_s)',            ~ %hash, "hash lookup");
todo_is(eval '&::($code_s).()',           code(),  "named sub lookup");

todo_ok(eval '!defined($::("nosuch"))',            "unknown scalar lookup");
todo_ok(eval '!defined($::("nosuch"))',            "unknown scalar lookup doesn't autovivify");

todo_ok(eval '!defined($::("MY::nosuch"))',        "unknown lexical lookup");
todo_ok(eval '!defined($::("MY::nosuch"))',        "unknown lexical lookup doesn't autovivify");

# (lvalue lexicals -- see below.)


# symtable hash syntax

todo_is(eval '%MY::{\'$\' ~ $lex_s}',       "bar",   "loopup of lexical in current scope - symtable");
todo_is(eval '%::{\'$\' ~ $global_s}',      $global, "lookup of global in default package - symtable");
todo_is(eval '%*Main::{\'$\' ~ $global_s}', $global, "lookup of global in *Main package - symtable");
todo_is(eval '~ %::{\'@\' ~ $ary_s}',       ~ @ary,  "array lookup - symtable");
todo_is(eval '~ %::{\'%\' ~ $hash_s}',      ~ %hash, "array lookup - symtable");
todo_is(eval '%::{\'&\' ~ $code_s}.()',     code(),  "named sub lookup - symtable");

todo_ok(eval '!defined(%::<nosuch>)',                "unknown scalar lookup");
todo_ok(eval '!defined(%::<nosuch>)',                "unknown scalar lookup doesn't autovivify");

todo_ok(eval '!defined(%MY::<nosuch>)',              "unknown lexical lookup");
todo_ok(eval '!defined(%MY::<nosuch>)',              "unknown lexical lookup doesn't autovivify");

{
	todo_fail("package keyword");
	eval 'package Other1;';

	todo_ok(eval '%:: eq $symhash',       "package declaration changes current package");

	our $new_global =         "It is I.";

	my $lex = "carrot";       # hiding "bar".
	todo_is(eval '$::("MY::$lex_s")',     "carrot",  "loopup of hiding lexical");

	todo_ok(eval '!defined(%::(\'$\' ~ $global_s))', "lookup of global in wrong package"); # XXX: error? warning? silent?
	my $a = eval '$::($global_s)';
	my $b = eval '$::("*Main::$global_s")';
    
	todo_ok (defined $a && defined $b && $a eq $b,   "package search");
	
}

todo_ok(defined eval '%::' && eval '%::' eq $symhash, "previous package declaration was scoped");
todo_is(eval '%::<Other1::$new_global>',  "It is I.", "Global in other package still visible");

