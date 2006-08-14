# Usage: perl rule2parsec.pl GrammarFile.grammar [out-dir/]
# The .grammar file is in Perl 6 syntax
# The .hs file is automatically generated under the out-dir/ (./ by default)

package Pugs::Compiler::Grammar;

use Pugs::Compiler::Rule;
use Pugs::Compiler::Token;
use Pugs::Compiler::Regex;
use base 'Pugs::Grammar::Base';

*pod = Pugs::Compiler::Token->compile(q(
  \= <'cut'> \N* [\n|$] |
  \N* [ $ | \n <pod> ]
))->code;

# space, comments or pod 
*ws = Pugs::Compiler::Token->compile(q(
  [  
    \# \N* [\n|$] |
    [^|\n] \= \N* [\n|$] <pod> |
    \s
  ]*
))->code;

*grammar_name = Pugs::Compiler::Token->compile(q( [ \w | \d | \: ]+ ))->code;

*rule_name = Pugs::Compiler::Token->compile(q( \w+ ))->code;

*block = Pugs::Compiler::Token->compile(q( \{ [ <block> | <-[}]> | \\\\\} ]* \} ))->code;

*mod = Pugs::Compiler::Token->compile(q#
    \: <rule_name> [ \( $<val> := (<-[)]>+) \) ]?
    {
       if($<val>[0]){
           return { $<rule_name> => $<val>[0] };
       }else{
           return { $<rule_name> => 1 };
       }
    } #)->code;

*definition =
Pugs::Compiler::Rule->compile(q($<kind>:=[<'rule'>|<'token'>|<'regex'>] <rule_name> <mod>* <block>
    {
        my $body = substr($<block>, 1, -1);
	my %opt;
	map{ @opt{keys %$_} = values %$_ }@{$<mod>}
	    if($<mod>[0]);

	return { 'kind' => $<kind>, 'name' => $<rule_name>, 'body' => $body, 'options' => \%opt };
    }
))->code;

*grammar = Pugs::Compiler::Rule->compile(q(<'grammar'> <grammar_name> \;[ <definition>]*{
	return { 'name' => $<grammar_name>, 'definitions' => \@{$<definition>} };
    }))->code;

package main;
use strict;
use warnings;
use IO::File;
use Pugs::Grammar::Rule;
use Pugs::Runtime::Match::Ratchet;
use Pugs::Emitter::Rule::Parsec qw( emit rule_rename );
use Data::Dumper;

my $source_file = shift(@ARGV);
my $source = slurp($source_file);
my $match  = Pugs::Compiler::Grammar->grammar($source);

die $0 . ': grammar file cannot be parsed' unless $match;

my $ret = $match->();
my $fh = create_haskell_file($ret->{name});

foreach(@{$ret->{definitions}}){
    my $def = $_->();
    my $tree = Pugs::Grammar::Rule->rule($def->{body});

    print $fh 'rule '
	. Pugs::Emitter::Rule::Parsec::rule_rename($def->{name})
	. ' = '
	. Pugs::Emitter::Rule::Parsec::emit({ }, $tree->{capture}, $def->{options})
	. "\n";
}

close $fh;

sub slurp {
    my $fh = IO::File->new(shift) || return;
    return join('', $fh->getlines);
}

sub create_haskell_file {
    my $module = shift;
    open FH, '>', ($module . '.hs'); # XXX
    return *FH;
}

__END__

=head1 NAME

rule2parsec.pl - Compile Perl6 Grammars to Haskell module with Parsec code

=head1 SYNOPSIS

  # The .grammar file is in Perl 6 syntax
  # The .hs file is automatically generated under the out-dir/ (./ by default)

  perl rule2parsec.pl GrammarFile.grammar [out-dir/]

=head1 DESCRIPTION

Used to convert grammars in Perl 6 syntax into Haskell module.

=head1 AUTHORS

The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 Rules Spec: L<http://dev.perl.org/perl6/doc/design/syn/S05.html>
The Parsec homepage: L<http://www.cs.uu.nl/~daan/parsec.html>

=head1 COPYRIGHT

Copyright 2006 by Shu-Chun Weng.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=cut

