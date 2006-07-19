use v6-alpha;
use Test;

use Perl6::Grammar;

=kwid

Testing self hosting rules.

=cut

plan 25;

if !eval('("a" ~~ /a/)') {
  skip_rest "skipped tests - rules support appears to be missing";
} else {

# Testing the parsing of verious forms of numbers

is(?("1"  ~~ /^<Int>$/),Bool::True,"1 is parsed as an integer");
is(?("-1" ~~ /^<Int>$/),Bool::True,"-1 is parsed as an integer");

is(?("1."  ~~ /^<Rat>$/),Bool::True,"1. is parsed as a Rat");
is(?("-1."  ~~ /^<Rat>$/),Bool::True,"-1. is parsed as a Rat");

is(?("0.1"  ~~ /^<Rat>$/),Bool::True,"0.1 is parsed as a Rat");
is(?("-0.1"  ~~ /^<Rat>$/),Bool::True,"0.1 is parsed as a Rat");

is(?(".1"  ~~ /^<Rat>$/),Bool::True,".1 is parsed as a Rat");
is(?("-.1"  ~~ /^<Rat>$/),Bool::True,".1 is parsed as a Rat");

is(?("10.01"  ~~ /^<decimal>$/),Bool::True,"10.01 is parsed as a decimal");

is(?("1e3" ~~ /^<Rat>$/),Bool::True,"1e3 is parsed as a rational");

is(?("10.01e3" ~~ /^<Rat>$/),Bool::True,"10.01e3 is parsed as a rational");

is(?("42_000" ~~ /^<Int>$/),Bool::True,"underscores are allowed");
is(?("42_127_000" ~~ /^<Int>$/),Bool::True,"Multiple undescores are allowed");
is(?("_42" ~~ /^<Int>$/),Bool::False,"Leading underscores a dissallowed");

is(?("0b100" ~~ /^<binary>$/),Bool::True,"0b100 (binary) is parsed as a binary");

is(?("0x100" ~~ /^<hex>$/),Bool::True,"0x100 (hex) is parsed as a hex");

is(?("0o100" ~~ /^<oct>$/),Bool::True,"0o100 (oct) is parsed as a oct");

is(?("Perl6" ~~ /^<identZZ>$/),Bool::True,"ids are parsed");
is(?("Perl6::rule" ~~ /^<identZZ>$/),Bool::True,"ids are parsed as fullid");
is(?("::rule" ~~ /^<identZZ>$/),Bool::True,"global ids are parsed as fullid");
is(?('$foo' ~~ /^<variable>$/),Bool::True,"scalars are parsed as variables");
is(?('@foo' ~~ /^<variable>$/),Bool::True,"arrays are parsed as variables");
is(?('%foo' ~~ /^<variable>$/),Bool::True,"hashes are parsed as variables");
is(?('&foo' ~~ /^<variable>$/),Bool::True,"subs are parsed as variables");
is(?('sub () {}' ~~ /^<anonsub>$/),Bool::True,"Anon subs parsing", :todo<bug>);

}
