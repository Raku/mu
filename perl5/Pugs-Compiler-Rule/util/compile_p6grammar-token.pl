# Usage: perl compile_p6grammar.pl GrammarFile.pm > GrammarFile.pmc
# The .pm file is in Perl 6 syntax
# The .pmc file is in Perl 5 Pugs::Compiler::Rule syntax

package Grammar::Compiler;
use Pugs::Compiler::Rule;
use base 'Pugs::Grammar::Base';

*grammar_name = Pugs::Compiler::Rule->compile(q( \S+ ))->code;
*rule_name = Pugs::Compiler::Rule->compile(q( \w+ ))->code;
*block = Pugs::Compiler::Rule->compile(q( \{ [<block>|<any>]*? \} ))->code;

*regex = Pugs::Compiler::Rule->compile(q(
    <'regex'> <ws> <rule_name> <ws> <block> <ws>
    {
	my $body = substr($<block>(), 1, -1);
	$body =~ s/\\\\/\\\\\\\\/g;  # duplicate every single backslashes
	return "*" . $<rule_name>() . " = Pugs::Compiler::Rule->compile(q(" .
	$body . "))->code;"
    }
))->code;

*token = Pugs::Compiler::Rule->compile(q(
    <'token'> <ws> <rule_name> <ws> <block> <ws>
    {
	my $body = substr($<block>(), 1, -1);
	$body =~ s/\\\\/\\\\\\\\/g;  # duplicate every single backslashes
	return "*" . $<rule_name>() . " = Pugs::Compiler::Rule->compile(q(" .
	$body . "), { ratchet => 1 } )->code;"
    }
))->code;

# TODO - :sigspace not implemented yet
*rule = Pugs::Compiler::Rule->compile(q(
    <'rule'> <ws> <rule_name> <ws> <block> <ws>
    {
	my $body = substr($<block>(), 1, -1);
	$body =~ s/\\\\/\\\\\\\\/g;  # duplicate every single backslashes
	return "*" . $<rule_name>() . " = Pugs::Compiler::Rule->compile(q(" .
	$body . "), { ratchet => 1, sigspace => 1 } )->code;"
    }
))->code;

*grammar = Pugs::Compiler::Rule->compile(q( grammar <ws> <grammar_name>\; <ws> [<rule>|<token>|<regex>]* { return "package " . $<grammar_name>() . ";\nuse Pugs::Compiler::Rule;\nuse base 'Pugs::Grammar::Base';\n\n" . join("\n", map { $_->() } @{$<rule>} ) . "\n" } ))->code;

package main;
use IO::File;

my $source_file = shift(@ARGV);
my $source = slurp($source_file);
my $match  = Grammar::Compiler->grammar($source);
print $match->();

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

