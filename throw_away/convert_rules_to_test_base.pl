#!/usr/bin/perl

use strict;
use warnings;
no warnings 'uninitialized';

use Tie::RefHash;
use Regexp::Common;
use String::Escape qw/quote/;
use Data::Dumper;

print <<HEADER;

use strict;
use warnings;
use Parrot::Test 'no_plan';
use Parrot::Test::PGE;

HEADER

tie my %exprs, 'Tie::RefHash', (
	qr{ \$ <> }x=> sub { compare => "whole_match" },
	qr{ \$ \/ \. from }x, sub { compare => "pos_whole_match_begin" },
	qr{ \$ (?: 0 | \/ ) (?: \[ -? \d+ \] )+ (?: \.from ) }x => sub {
		my @slice = ($_[0] =~ /\[ (-? \d+) \]/gx);
		my $pos = ($_[0] =~ /from/);
		match_var => "@slice", compare => ($pos ? "match_var" : "pos_match_var_begin");
	},
	qr{ \$ \d+ }x => sub {
		$_[0] =~ /^\$(\d+)$/;
		match_var => $1, compare => "match_var";
	}
);

my $exprs = do { my $x = join("|", keys %exprs); qr/\s* ($x) \s*/x };

my $str = qr/\s* ( $RE{quoted} ) \s*/x;
my $val = qr/\s* ( \d+ | $RE{quoted} ) \s*/x;
my $rx_decl = qr/(?: rx(:i)? )?/x;
my $re = qr{\s* $rx_decl ( $RE{delimited}{-delim => '/'} ) \s*}x;
my $comma = qr/\s* , \s*/x;
my $sm = qr/~~/;

my $todo = qr/(?: $comma :todo<(feature|bug)> )?/x;

tie my %comparisons, 'Tie::RefHash', (
	qr/use (v6|Test)|plan \d+/ => sub { },
	
	qr/ is \s* \( \s* \( $str ~~ $re && $exprs \) $comma $val $comma $str $todo \) \s* ; /x => sub {
		my ( $line, $const, $opt, $pat, $expr, $eq, $desc, $todo ) = @_;

		foreach my $ep (keys %exprs) {
			if ($expr =~ $ep){
				$expr = { $exprs{$ep}->($expr) };
				last;
			}
		};

		printf "p6rule_like($const, '%s', qr{%s}, $desc%s);\n", unre($pat), expr_desc_to_qr($expr, quotemeta(unquote($eq))), maybe_todo($todo);
	},
	qr/ ok \s* \( \s* \( \s* not \s* \( $str ~~ $re \) \s* \) $comma $str $todo \) \s* ; /x => sub {
		my ($line, $const, $opt, $pat, $desc, $todo) = @_;
		printf "p6rule_isnt($const, '%s', $desc%s);\n", unre($pat), maybe_todo($todo);
	},
	qr/ ok \s* \( \s* \( $str ~~ $re \) $comma $str $todo \) \s* ;/x => sub {
		my ($line, $const, $opt, $pat, $desc, $todo) = @_;
		printf "p6rule_is($const, '%s', $desc%s);\n", unre($pat), maybe_todo($todo);
	},
	qr/ fail \s* \( $str $todo \);/x => sub {
		my ($line, $reason, $todo) = @_;
		print "#NOTREADY; # $line\n";
	}
);

LINE: while (<ARGV>) {
	print, next if /^\s*(#|$)/;
	chomp;
	foreach my $comparison (keys %comparisons) {
		if (/$comparison/) {
			my $line = $_;
			$comparisons{$comparison}->(
				$line,
				map { substr($line, $-[$_], $+[$_] - $-[$_]) } 1 .. $#-,
			);
			next LINE;
		}
	}
	#warn "couldn't match $_ against any pattern";
	print "###FIXME### $_\n"
}

sub unquote {
	my $str = shift;

	if ($str =~ /^'(.*)'$/) {
		my $str = $1;
		$str =~ s/\\'/'/g;
		return $str;
	} else {
		return String::Escape::unquote($str);
	}
}

sub unre {
	$_[0] =~ $RE{delimited}{-delim => '/'}{-keep} or die "can't remove regex delims on something that doesn't look like a regex";
	$3;
}

sub expr_desc_to_qr {
	my $desc = shift;
	my $against = shift;

	my $mv_ident = "mob ".($desc->{match_var}). ":";
	my $whole_match = "mob:";

	my %descs = (
		whole_match => sub { spec_is($whole_match, $against) },
		match_var => sub { spec_is($mv_ident, $against) },
		pos_whole_match_begin => sub { pos_is($whole_match, $against) },
		pos_match_var_begin => sub { pos_is($mv_ident, $against) },
	);

	if (my $c = $desc->{compare}) {
		return ($descs{$c} || die "no handler for $c")->();
	} else {
		die @_;
	}
}

sub spec_is { qr/$_[0] <$_[1] @ \d+> \d+/ }

sub pos_is { qr/$_[0] <.*? @ $_[1]> \d+/ }

sub maybe_todo {
	my $reason || return '';
	quote($reason);
}
