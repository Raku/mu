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
	my $kind = "$<kind>";
        my $body = substr($<block>, 1, -1);
	my %opt;
	map{ @opt{keys %$_} = values %$_ }@{$<mod>}
	    if($<mod>[0]);

	$opt{'sigspace'} = 1 if $kind eq 'rule';

	return { 'kind' => $kind, 'name' => "$<rule_name>", 'body' => $body, 'options' => \%opt };
    }
))->code;

*using = Pugs::Compiler::Rule->compile(q(<'use'> <grammar_name> \;{
	return { 'module' => "$<grammar_name>" };
    }))->code;

*ending = Pugs::Compiler::Rule->compile(q(<'use'> <'Haskell'> \;
    $<body>:=[.*]{
	return "$<body>"
    }))->code;

*grammar = Pugs::Compiler::Rule->compile(q(<'grammar'> <grammar_name>
	\;[ [<definition>|<ending>|<using>]]*{
	return { 'name'       => "$<grammar_name>",
		'definitions' => \@{$<definition>},
		'uses'        => \@{$<using>},
		'ending'      => $<ending>[0] ? "$<ending>[0]" : ''
	};
    }))->code;

package main;
use strict;
use warnings;
use IO::File;
use File::Path;
use File::Spec;
use Pugs::Grammar::Rule;
use Pugs::Runtime::Match::Ratchet;
use Pugs::Emitter::Rule::Parsec qw( emit rule_rename );
use Data::Dumper;

my $source_file = shift(@ARGV);
my $out_dir = $ARGV[0] ? shift : '.';
my $source = slurp($source_file);
my $match  = Pugs::Compiler::Grammar->grammar($source);

die $0 . ': grammar file cannot be parsed' unless $match;

my $ret = $match->();
my $fh = create_haskell_file($ret->{name});

foreach(@{$ret->{uses}}){
    my $uses = $_->();
    print $fh 'import ' . perl_module_to_haskell($uses->{module}) . "\n";
}
print $fh "\n";

foreach(@{$ret->{definitions}}){
    my $def = $_->();
    my $tree = Pugs::Grammar::Rule->rule($def->{body});

    print $fh Pugs::Emitter::Rule::Parsec::rule_rename($def->{name})
	. ' = '
	. Pugs::Emitter::Rule::Parsec::emit({ }, $tree->{capture}, $def->{options})
	. "\n";
}

print $fh "-- Begin of copied functions\n";
print $fh $ret->{ending};

close $fh;

sub perl_module_to_haskell {
    local $_ = shift;
    s/::/./g;
    return $_;
}

sub slurp {
    my $fh = IO::File->new(shift) || return;
    return join('', $fh->getlines);
}

sub create_haskell_file {
    my $module = perl_module_to_haskell shift;
    my($path, $dir);

    $path = $module;
    $path   =~ s/\./\//g;

    $dir = File::Spec->catdir($out_dir,
	    substr $path, 0, 1 + rindex($path, '/'));

    mkpath $dir if $dir ne '';

    open FH, '>', ($path . '.hs'); # XXX
    print FH <<HEADER ;
{-# OPTIONS_GHC -fglasgow-exts #-}
module $module where

HEADER
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

