#!/usr/bin/pugs

use v6;
use Test;

plan 11;

=pod

Testing lvalue-returning subroutines

L<S06/"Lvalue subroutines">

=cut

my $val1 = 1;
my $val2 = 2;

sub lastval is rw { return $val2; }
sub prevval is rw { return lastval(); }

lastval() = 3;
is($val2, 3); # simple

prevval() = 4;
is($val2, 4); # nested

# S6 says that lvalue subroutines are marked out by 'is rw'
sub notlvalue { return $val1; } # without rw

notlvalue() = 5;
is $val1, 1, 'non-rw subroutines should not support assignment', :todo<bug>;
isnt $val1, 5, 'non-rw subroutines should not assign', :todo<bug>;

sub check ($passwd) { return $password eq "fish"; };

eval 'sub checklastval ($passwd) is rw {
	my $proxy is Proxy(
		FETCH => sub ($self) {
			    return lastval();
			 },
		STORE => sub ($self, $val) {
			    die "wrong password" unless check($passwd);
			    lastval() = $val;
			 }
        );
	return $proxy;
};';

my $errors;
eval 'try { checklastval("octopus") = 10 }; $errors=$!;';
is($errors, "wrong password", 'checklastval STORE can die', :todo<feature>);

# Above test may well die for the wrong reason, if the Proxy stuff didn't
# parse OK, it will complain that it couldn't find the desired subroutine
eval_is('checklastval("fish") = 12; $val2', 12, 'proxy lvalue subroutine STORE works', :todo<feature>);
my $resultval;
eval '$resultval = checklastval("fish");';
is($resultval, 12, 'proxy lvalue subroutine FETCH works', :todo<feature>);

my $realvar = "FOO";
eval_ok 'sub proxyvar ($var) is rw {
	    return new Proxy:
		FETCH => { lc($realvar) },
		STORE => { lc($realvar = $^val) };
        }', 'defining lvalue sub using `new Proxy: ` works', :todo<feature>;
eval_is 'proxyvar()', 'foo', 'proxy lvalue subroutine FETCH works', :todo<feature>;
eval_is 'proxyvar("BAR")', 'bar', 'proxy lvalue subroutine STORE works', :todo<feature>;
is $realvar, 'BAR', 'variable was modified', :todo<feature>;
