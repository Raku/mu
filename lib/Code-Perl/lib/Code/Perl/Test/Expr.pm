# $Header: /home/fergal/my/cvs/Code-Perl/lib/Code/Perl/Test/Expr.pm,v 1.1 2003/06/17 15:14:55 fergal Exp $

use strict;

use warnings;

package Code::Perl::Test::Expr;

use Test::More;

use Test::Deep qw( cmp_deeply );
use Data::Dumper qw(Dumper);
use Carp;

my $do_not_run = {};

sub do_not_run
{
	return $do_not_run;
}

sub listcon
{
	return ListCon->new(@_);
}

sub test_exprs
{
	my @tests = @_;
	my $test_count = 0;
	for my $test (@tests)
	{
		$test_count++;
		my ($name, $env, $comp, $exp_value, $exp_code) = @$test;

		$name = "$test_count: $name";
		my $code = eval {$comp->perl};
		die "$@\n".Dumper($comp) if $@;

		SKIP:
		{
			if (defined $exp_code)
			{
				is($code, $exp_code, "$name - code") || diag(Dumper($comp));
			}
			else
			{
				skip("no code supplied", 1);
			}
		}

		SKIP:
		{
			if ($exp_value eq $do_not_run)
			{
				skip("do not run", 3);
			}
			else
			{
				my $list_con = ref($exp_value) eq "ListCon";
				
				$main::env = $env;

				my $eval_value = $list_con ? listcon($comp->eval) : $comp->eval;
				cmp_deeply($eval_value, $exp_value, "$name - value") || diag(Dumper($comp));

				my $perl_value = $list_con ? listcon(eval $code) : eval $code;

				ok(! $@, "$name - evalperl") || diag "$@\ncode was\n$code";
				cmp_deeply($perl_value, $exp_value, "$name - evalperl value") || diag($code);
			}
		}

	}
}

package ListCon;

sub new
{
	my $pkg = shift;

	my @list = @_;
	return bless \@list, $pkg;
}

1;
