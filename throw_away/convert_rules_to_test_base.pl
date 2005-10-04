#!/usr/bin/perl

use strict;
use warnings;
no warnings 'uninitialized';

use Tie::RefHash;
use Regexp::Common;
use String::Escape ();

tie my %exprs, 'Tie::RefHash', (
	qr{ \$ <> }x=> sub { compare => "match" },
	qr{ \$ \/ \. from }x, sub { compare => "pos_match_begin" },
	qr{ \$ (?: 0 | \/ ) \[ \d+ \] \. from }x => sub { match_var => $1 + 1, compare => "pos_match_begin"},
	qr{ \$ (?: 0 | \/ ) \[ -1  \] \. from }x => sub { match_var => "last", compare => "pos_match_begin"},
	qr{ \$ (?: 0 | \/ ) \[ \d+ \] }x => sub { match_var => $1 + 1, compare => "match_var"},
	qr{ \$ (?: 0 | \/ ) \[ -1  \] }x => sub { match_var => "last", compare => "match_var"},
	qr{ \$ \d+ }x => sub { match_var => $1 + 1, compare => "match_var" }
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
	qr/ is \s* \( \s* \( $str ~~ $re && $exprs \) $comma $val $comma $str $todo \) \s* ; /x => sub {
		my ( $line, $const, $opt, $pat, $expr, $eq, $desc, $todo ) = @_;

		$const = unquote($const);
		$desc  = unquote($desc);
		$pat   = unre($pat);

		foreach my $ep (keys %exprs) {
			if ($expr =~ $ep){
				$expr = { $exprs{$ep}->() };
				last;
			}
		};

		print "'$const' ~= /$pat/ and ($expr->{compare} eq $eq). desc='$desc' todo=$todo # $line\n";
	},
	qr/ ok \s* \( \s* \( \s* not \s* \( $str ~~ $re \) \s* \) $comma $str $todo \) \s* ; /x => sub {
		my ($line, $const, $opt, $pat, $desc, $todo) = @_;

		$const = unquote($const);
		$desc  = unquote($desc);
		$pat   = unre($pat);

		print "'$const' !~ /$pat/. desc='$desc' todo=$todo # $line\n";
	},
	qr/ ok \s* \( \s* \( $str ~~ $re \) $comma $str $todo \) \s* ;/x => sub {
		my ($line, $const, $opt, $pat, $desc, $todo) = @_;
		
		$const = unquote($const);
		$desc  = unquote($desc);
		$pat   = unre($pat);

		print "'$const' ~= /$pat/. desc='$desc' todo=$todo # $line\n";
	},
	qr/ fail \s* \( $str $todo \);/x => sub {
		my ($line, $reason, $todo) = @_;
		print "fail; # $line\n";
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
		return String::Escape::unquote($1);
	}
}

sub unre {
	$_[0] =~ $RE{delimited}{-delim => '/'}{-keep} or die "can't remove regex delims on something that doesn't look like a regex";
	$3;
}
