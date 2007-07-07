#!/usr/bin/env perl
# The .pm file is in Perl 6 syntax
# The .pmc file is in Perl 5 Pugs::Compiler::Rule syntax

# TODO - in order to Grammar.pm to self-compile:
#    unshift @rule_terms, 'dot';

use lib 'lib';
use File::Slurp 'slurp';
use Pugs::Compiler::Grammar;

my $infile = shift or
    die "Usage: $0 foo.grammar > Foo.pm\n";
my $grammar = slurp($infile);
my $compiler = Pugs::Compiler::Grammar->compile($grammar) or
    die;
print $compiler->perl5 if $compiler;

__END__

=head1 NAME

compile_p6grammar.pl - Compile Perl6 Grammars to Perl5 Modules

=head1 SYNOPSIS

  # The .pm file is in Perl 6 syntax
  # The .pmc file is in Perl 5 Pugs::Compiler::Rule syntax

  perl compile_p6grammar.pl foo.grammar > Foo.pm

=head1 DESCRIPTION

Used to convert grammars in Perl 6 syntax into Perl 5 modules.

=head1 AUTHORS

The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 Rules Spec: L<http://dev.perl.org/perl6/doc/design/syn/S05.html>

=head1 COPYRIGHT

Copyright 2006 by Nathan Gray and Agent Zhang.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=cut

