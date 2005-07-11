#!/usr/bin/pugs

use v6;
use Test;

use Perl6;

=kwid

Testing self hosting rules.

=cut

plan 25;

# Testing the parsing of verious forms of numbers

is(?("1"  ~~ /^<Int>$/),bool::true,"1 is parsed as an integer");
is(?("-1" ~~ /^<Int>$/),bool::true,"-1 is parsed as an integer");

is(?("1."  ~~ /^<Rat>$/),bool::true,"1. is parsed as a Rat");
is(?("-1."  ~~ /^<Rat>$/),bool::true,"-1. is parsed as a Rat");

is(?("0.1"  ~~ /^<Rat>$/),bool::true,"0.1 is parsed as a Rat");
is(?("-0.1"  ~~ /^<Rat>$/),bool::true,"0.1 is parsed as a Rat");

is(?(".1"  ~~ /^<Rat>$/),bool::true,".1 is parsed as a Rat");
is(?("-.1"  ~~ /^<Rat>$/),bool::true,".1 is parsed as a Rat");

is(?("10.01"  ~~ /^<decimal>$/),bool::true,"10.01 is parsed as a decimal");

is(?("1e3" ~~ /^<Rat>$/),bool::true,"1e3 is parsed as a rational");

is(?("10.01e3" ~~ /^<Rat>$/),bool::true,"10.01e3 is parsed as a rational");

is(?("42_000" ~~ /^<Int>$/),bool::true,"underscores are allowed");
is(?("42_127_000" ~~ /^<Int>$/),bool::true,"Multiple undescores are allowed");
is(?("_42" ~~ /^<Int>$/),bool::false,"Leading underscores a dissallowed");

is(?("0b100" ~~ /^<binary>$/),bool::true,"0b100 (binary) is parsed as a binary");

is(?("0x100" ~~ /^<hex>$/),bool::true,"0x100 (hex) is parsed as a hex");

is(?("0o100" ~~ /^<oct>$/),bool::true,"0o100 (oct) is parsed as a oct");

is(?("Perl6" ~~ /^<ident>$/),bool::true,"ids are parsed");
is(?("Perl6::rule" ~~ /^<ident>$/),bool::true,"ids are parsed as fullid");
is(?("::rule" ~~ /^<ident>$/),bool::true,"global ids are parsed as fullid");
is(?('$foo' ~~ /^<variable>$/),bool::true,"scalars are parsed as variables");
is(?('@foo' ~~ /^<variable>$/),bool::true,"arrays are parsed as variables");
is(?('%foo' ~~ /^<variable>$/),bool::true,"hashes are parsed as variables");
is(?('&foo' ~~ /^<variable>$/),bool::true,"subs are parsed as variables");
#ok('sub () {}' ~~ /^<anonsub>$/,"Anon subs parsing");

