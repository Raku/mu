use 5.006001;
use strict;
use warnings;

package Pugs::Compiler::Rule;

our $VERSION = '0.32';

use base 'Pugs::Compiler::Regex';

sub compile {
    my ( $class, $rule_source, $param ) = @_;
    $param = ref $param ? { %$param } : {};
    $param->{ratchet} = 1
        unless exists $param->{ratchet};
    $param->{sigspace} = 1
        unless exists $param->{sigspace} ||
               exists $param->{s};
    $class->SUPER::compile( $rule_source, $param );
}

1;
__END__

=head1 NAME

Pugs::Compiler::Rule - Compiler for Perl 6 regexes

=head1 VERSION

This document describes Pugs::Compiler::Rule 0.28 released
on 31 Oct, 2007.

=head1 SYNOPSIS

Un-named rules are objects:

    use Pugs::Compiler::Rule;

    my $rule = Pugs::Compiler::Rule->compile( '((.).).' );
    my $match = $rule->match( 'abc' );

    if ($match) {               # true
        print $match;           # "abc"
        print $match->from;     # 0
        print $match->to;       # 3
        print $match->[0];      # "ab"
        print $match->[0][0];   # "a"
    }

Named rules are methods in a Grammar:

    package MyGrammar;
    use Pugs::Compiler::Rule;
    use base 'Pugs::Grammar::Base';

    Pugs::Compiler::Rule->install( rule => '((.).).' );
    my $match = MyGrammar->rule( 'abc' );

Rules may have parameters:

    $grammar->install(subrule => $source, { signature => $sig } );

    $grammar->install(rule => q{
            <subrule: param1, param2>
    });

where C<$grammar> is normally a Perl 5 package.

=head1 DESCRIPTION

This module provides an pure Perl 5 implementation for Perl 6 regexes,
which does not depend on the Haskell Pugs.

It is a front-end to several other modules:

Front-end Modules

=over 4

=item * L<Pugs::Compiler::Grammar> compiles Perl 6 grammars to Perl 5.

=item * L<Pugs::Compiler::Rule> compiles Perl 6 rules to Perl 5.

=item * L<Pugs::Compiler::Token> compiles Perl 6 tokens to Perl 5.

=item * L<Pugs::Compiler::Regex> compiles Perl 6 regexes to Perl 5.

=item * L<Pugs::Compiler::RegexPerl5> wraps Perl 5 regexes to return a B<Match> object.

=back

Runtime Classes

=over 4

=item * L<Pugs::Runtime::Rule> provides the runtime engine for Rules.

=item * L<Pugs::Runtime::Match> represents a B<Match> object.

=item * L<Pugs::Runtime::Grammar> represents a B<Grammar> class / object.

=back

Grammars

=over 4

=item * L<Pugs::Grammar::Rule> parses the Rules syntax.

=item * L<Pugs::Grammar::Base> is the base Grammar: <ws>, <space>.

=back

Code Emitters

=over 4

=item * L<Pugs::Emitter::Rule::Perl5> converts parsed Rules to Perl 5 code.

=item * L<Pugs::Emitter::Rule::Perl5::Ratchet> converts parsed :ratchet Rules to Perl 5 code.

=item * L<Pugs::Emitter::Grammar::Perl5> converts parsed grammars to Perl 5 code.

=back

=head1 INHERITANCE

  Pugs::Compiler::Rule
     isa Pugs::Compiler::Regex

=head1 METHODS

This class (i.e. L<Pugs::Compiler::Rule>) is a
subclass of L<Pugs::Compiler::Regex> and thus owns
all the methods of its base class. See
L<Pugs::Compiler::Regex> for the detailed docs.

=over

=item C<< $rule = Pugs::Compiler::Rule->compile($p6_regex, $params) >>

Specifically, this class overrides the C<compile>
method of L<Pugs::Compiler::Regex> which resets
the following options' default values:

=over

=item C<< ratchet => 1 >>

Here is an example:

    $rule = Pugs::Compiler::Rule->compile(
        'a*\w',
    );
    my $match = $rule->match('aaa');
    # $match->bool is false since no backtracking
    # happened

=item C<< sigspace => 1 >>

Here is an example:

    my $rule = Pugs::Compiler::Rule->compile(
        'a b',
    );
    my $match = $rule->match('a     b');
    ok $match->bool, 'sigspace works';
    is $match->(), 'a     b', 'sigspace works (2)';

=back

=back

=head1 CAVEATS

This is an experimental development version. The API is still in flux.

The set of implemented features depend on the C<ratchet> switch.

=head1 AUTHORS

The Pugs Team C<< <perl6-compiler@perl.org> >>.

Please join us on irc.freenode.net C<#perl6> if you'd like to participate.

=head1 SEE ALSO

=over

=item *

L<Pugs::Compiler::Regex>

=item *

L<Pugs::Compiler::Grammar>

=item *

L<compile_p6grammar.pl>

=item *

The Perl 6 Rules Spec: L<http://perlcabal.org/syn/S05.html>

=back

=head1 COPYRIGHT

Copyright 2006, 2007 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=cut

