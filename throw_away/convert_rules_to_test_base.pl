#!/usr/bin/perl

use strict;
use warnings;
no warnings 'uninitialized';

use Tie::RefHash;
use Regexp::Common;
use String::Escape qw/quote/;
use Data::Dumper;
use List::MoreUtils qw/any/;

print <<HEADER;

use strict;
use warnings;
use Parrot::Test 'no_plan';
use Parrot::Test::PGE;

HEADER

my $var = qr{ \$ (?: \d+ | / ) }x;
my $slices = qr/(?: \[-?\d+\] )*/x;
my $from = qr/(?: \.from )?/x;

tie my %exprs, 'Tie::RefHash', (
	qr{ \$ <> }x=> sub { compare => "whole_match" },
	qr{ $var $slices $from }x => sub {
		warn "matched an expr: $_[0]";
		my @slice = ($_[0] =~ /(-?\d+)/g);
		my $pos = ($_[0] =~ /\.from/);
		my $unit = @slice ? "match_var" : "whole_match";
		warn "slice=@slice pos=$pos unit=$unit";
		my @res = (
			(@slice ? (match_var => \@slice) : ()),
			compare => ($pos ? "pos_${unit}_begin" : $unit)
		);
		warn "res=@res";
		@res;
	},
);

my $exprs = do { my $x = join("|", keys %exprs); qr/\s* ($x) \s*/x };
warn "$exprs";

my $str = qr/\s* ( $RE{quoted} ) \s*/x;
my $val = qr/\s* ( \d+ | $RE{quoted} ) \s*/x;
my $rx_decl = qr/(?: rx(?: :([i]+) )? )?/x;
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
		}

		printf "p6rule_like($const, '%s', qr{%s}, $desc, opt => '$opt', %s);\n", rx($pat), expr_desc_to_qr($expr, quotemeta(unquote($eq))), maybe_todo($todo);
	},
	qr/ ok \s* \( \s* \( \s* not \s* \( $str ~~ $re \) \s* \) $comma $str $todo \) \s* ; /x => sub {
		my ($line, $const, $opt, $pat, $desc, $todo) = @_;
		printf "p6rule_isnt($const, '%s', $desc%s);\n", rx($pat), maybe_todo($todo);
	},
	qr/ ok \s* \( \s* \( $str ~~ $re \) $comma $str $todo \) \s* ;/x => sub {
		my ($line, $const, $opt, $pat, $desc, $todo) = @_;
		printf "p6rule_is($const, '%s', $desc%s);\n", rx($pat), maybe_todo($todo);
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


	my $mv_ident = match_var_qr($desc->{match_var});
	my $whole_match = "mob:";

	my %descs = (
		whole_match => sub { spec_is($whole_match, $against) },
		match_var => sub { spec_is($mv_ident, $against) },
		pos_whole_match_begin => sub { pos_is($whole_match, $against) },
		pos_match_var_begin => sub { pos_is($mv_ident, $against) },
	);

	if (my $c = $desc->{compare}) {
		my $qr = ($descs{$c} || die "no handler for $c")->();
	} else {
		die @_;
	}
}

sub match_var_qr {
	my @spec = @{ shift || return ""};
	my $prefix = "mob";
	my $suffix = ":";

	if ($spec[-1] == -1) {
		pop @spec;
		my $match = join(" ", $prefix, @spec, "\\d+") . $suffix;
		return "$match(?!$match)";
	} elsif (any { $_ == -1 } @spec) {
		die "can't emit @spec";
	} else {
		return join(" ", $prefix, @spec) . $suffix;
	}
}

sub spec_is {
	if ($_[1] ne "") {
		return qr/$_[0] <$_[1] @ \d+> \d+/;
	} else {
		return qr/(?!$_[0] <)|$_[0] < @/;
	}
}

sub pos_is { qr/$_[0] <.*? @ $_[1]> \d+/ }

sub maybe_todo {
	my $reason = shift || return '';
	", todo => " . quote($reason);
}

sub rx {
	my $pat = unre(shift);
	$pat;
}
