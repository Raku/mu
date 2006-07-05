# Usage: perl compile_p6grammar.pl GrammarFile.pm > GrammarFile.pmc
# The .pm file is in Perl 6 syntax
# The .pmc file is in Perl 5 Pugs::Compiler::Rule syntax

# TODO - in order to Grammar.pm to self-compile:
#    unshift @rule_terms, 'dot';

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

*mod = Pugs::Compiler::Token->compile(q( \: <rule_name> { return $<rule_name> . " => 0" } ))->code;

*rule = Pugs::Compiler::Rule->compile(q(<'rule'> <rule_name> <mod>* <block>
    {
        my $body = substr($<block>, 1, -1);
        my $mod = $<mod>[0] ? ", { " . join(", ", @{$<mod>}) . " }" : "";
        $body =~ s/\\\\/\\\\\\\\/g;  # duplicate every single backslashes
        return "*" . $<rule_name> . " = Pugs::Compiler::Rule->compile(q(" .
        $body . ")$mod)->code;"
    }
))->code;
*token = Pugs::Compiler::Rule->compile(q(<'token'> <rule_name> <mod>* <block>
    {
        my $body = substr($<block>, 1, -1);
        my $mod = $<mod>[0] ? ", { " . join(", ", @{$<mod>}) . " }" : "";
        $body =~ s/\\\\/\\\\\\\\/g;  # duplicate every single backslashes
        $body =~ s/([\(\)])/\\\\$1/g;  # escape ( and ) as we are in q()
        return "*" . $<rule_name> . " = Pugs::Compiler::Token->compile(q(" .
        $body . ")$mod)->code;"
    }
))->code;
*regex = Pugs::Compiler::Rule->compile(q(<'regex'> <rule_name> <mod>* <block>
    {
        my $body = substr($<block>, 1, -1);
        my $mod = $<mod>[0] ? ", { " . join(", ", @{$<mod>}) . " }" : "";
        $body =~ s/\\\\/\\\\\\\\/g;  # duplicate every single backslashes
        return "*" . $<rule_name> . " = Pugs::Compiler::Regex->compile(q(" .
        $body . ")$mod)->code;"
    }
))->code;

# --- compile Rule.pm internal stuff
*p6_stuff = Pugs::Compiler::Rule->compile(q((unshift|push) <[@]>(\w+), <[']>(\w+)<[']>;
    {
        return $/[0] . ' @' . $/[1] . ", '" . $/[2] . "';";
    }
))->code;

# ---

*grammar = Pugs::Compiler::Rule->compile(q( <'grammar'> <grammar_name> \;[ [<rule>|<token>|<regex>|<p6_stuff>]]* {
        return "package $<grammar_name>;\n" .
            "use Pugs::Compiler::Rule;\n" .
            "use Pugs::Compiler::Token;\n" .
            "use Pugs::Compiler::Regex;\n" .
            "use base 'Pugs::Grammar::Base';\n" .
            "use Pugs::Runtime::Match::Ratchet; # overload doesn't work without this ???\n\n" .
            join("\n", ( 
                map { "$_" } @{$<regex>},
                map { "$_" } @{$<token>},
                map { "$_" } @{$<rule>},
                map { "$_" } @{$<p6_stuff>},
            )) . "\n"
    }))->code;

package main;
use IO::File;
use Pugs::Runtime::Match::Ratchet;

my $source_file = shift(@ARGV);
my $source = slurp($source_file);
my $match  = Pugs::Compiler::Grammar->grammar($source);
print "$match";

sub slurp {
    my $fh = IO::File->new(shift) || return;
    return join('', $fh->getlines);
}

__END__

=head1 NAME

compile_p6grammar.pl - Compile Perl6 Grammars to Perl5 Modules

=head1 SYNOPSIS

  # The .pm file is in Perl 6 syntax
  # The .pmc file is in Perl 5 Pugs::Compiler::Rule syntax

  perl compile_p6grammar.pl GrammarFile.pm > GrammarFile.pmc

=head1 DESCRIPTION

Used to convert grammars in Perl 6 syntax into Perl 5 modules.

=head1 AUTHORS

The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 Rules Spec: L<http://dev.perl.org/perl6/doc/design/syn/S05.html>

=head1 COPYRIGHT

Copyright 2006 by Nathan Gray.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=cut

