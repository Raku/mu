#!?usr/bin/perl
use PadWalker qw(peek_my peek_our);
use warnings;
use strict;
sub ruby_quote($) {
	my ($string) = @_;
	my %hash = (%{peek_our(1)},%{peek_my(1)});
	$string =~ s!(\#\{  (.*?)  \})!${$hash{$2}||\$1}!xg;

	#XXX: LexAlias is required to make it work properly
	$string =~ s!\#\{  (.*?)  \}!eval($1)!exg;
	return $string;
}
my  $lang = "Perl";
our $lang2 = "Ruby";
print ruby_quote '$vars are sometimes used both in #{$lang} and in #{$lang2}, but #{1+1} is a sigil in neither
';
