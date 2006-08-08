package Pugs::Compiler::Rule;
$Pugs::Compiler::Rule::VERSION = '0.12_01';

# Documentation in the __END__
use 5.006;
use strict;
use warnings;

use base 'Pugs::Compiler::Regex';

sub compile {
    my ( $class, $rule_source, $param ) = @_;
    $param = ref $param ? { %$param } : {}; 
    $param->{ratchet} = 1 
        unless defined $param->{ratchet};
    $param->{sigspace} = 1 
        unless defined $param->{sigspace} ||
               defined $param->{s};
    $class->SUPER::compile( $rule_source, $param );   
}

1;

__END__

=head1 NAME 

Pugs::Compiler::Rule - Compiler for Perl 6 Rules

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

=head1 DESCRIPTION

This module provides an implementation for Perl 6 Rules.  It is a front-end
to several other modules:

=over 4

* Front-end Modules

=item * L<Pugs::Compiler::Rule> compiles Perl 6 Rules to Perl 5.

=item * L<Pugs::Compiler::Token> compiles Perl 6 Tokens to Perl 5.

=item * L<Pugs::Compiler::Regex> compiles Perl 6 Regexes to Perl 5.

=item * L<Pugs::Compiler::RegexPerl5> wraps Perl 5 Regexes to return a B<Match> object.

* Runtime Classes

=item * L<Pugs::Runtime::Rule> provides the runtime engine for Rules.

=item * L<Pugs::Runtime::Match> represents a B<Match> object.

=item * L<Pugs::Runtime::Grammar> represents a B<Grammar> class / object.

* Grammars

=item * L<Pugs::Grammar::Rule> parses the Rules syntax.

=item * L<Pugs::Grammar::Rule::Rule> specifies the Rules syntax with Rules.

=item * L<Pugs::Grammar::Base> is the base Grammar: <ws>, <space>.

* Code Emitters

=item * L<Pugs::Emitter::Rule::Perl5> converts parsed Rules to Perl 5 code.

=item * L<Pugs::Emitter::Rule::Perl5::Ratchet> converts parsed :ratchet Rules to Perl 5 code.

=back

=head2 Implemented Features

  <ws> 
  #comment\n 
  . 
  ? * + *? +? ??
  literal
  [] 
  ()     
  |
 
  <'literal'>
  <subrule>
  <namespace::subrule>
  <$var>      
  <?subrule>
  <!subrule>
  <before ...>
  <after ...>         -- implemented in :ratchet mode only
  <subrule('param')>  -- constant parameters only
 
  %hash
 
  <@var>             -- special-cased for array-of-rule (but not Rule|Str)
  \char              -- not all chars implemented
  {code}             -- non-capturing closure
                     -- perl5 syntax inside closure
                     -- $/ doesn't work yet
  { return code }    -- capturing closure
                     -- perl5 syntax inside closure
                     -- $/ works
  $var := (capture)  -- capture aliasing
  $<> $/<>           -- special variables can't be used inside a match yet
  $/ 
  $<0> $<1>
  $0 $1
  \n \N
  $^a $^b            -- positional parameters (must start with $^a)
  ^ $
  :

=head2 Unimplemented or untested features

  $variable 
  @variable
  $/<0> $/<1>
  $/0 $/1
  <"literal">
  ^^ $$
  <unicode-class> <+unicode-class> <+unicode-class+unicode-class>
  <&var> 
  <%var>
  **{n..m}
  :: :::   (commit)
  $var := [non-capture]
  $var := <rule>
  <(closure-assertion)> <{code-returns-rule}>
  <<character-class>> <[character-class]>
  :flag :flag() :flag[]
  \x0a \0123 ...
  &    
  $1      - lvalue match variables
  $/ $()  - global match variables 

=head1 METHODS

=over

=item compile (Str $rule_source, \%options)

Class method.  Returns a compiled rule object, or throws an exception on
invalid rule syntax.

options:

=over

=item * grammar => $class

Specify which namespace (Grammar) the rule belongs to.

=item * ratchet => 1

Disable backtracking. Match faster. Defaults to 1 in Rules and Tokens.

=item * pos => $pos

Specify a string position to match. Starts in zero. Defaults to C<undef>, which matches anywhere in the string.

=item * sigspace => 1

Whitespace is significant. Defaults to 1 in Rules.

=item * Perl5 => 1

Use Perl 5 grammar and semantics for Regex.

=back

=item match (Str $match_against)

Instance method.  Returns a L<Pugs::Runtime::Match> object.

=item install (Str $name, Str $rule_source, \%options)

Install a rule into the method C<$name>. If C<$name> is fully qualified
then it will be installed into that path e.g C<MyGrammar::rulename>,
otherwise it will install it into the current package.

=item perl

Instance method.  Returns a string that can be eval'ed into a
rule/token/regex object.

=back

=head1 CAVEATS

This is an experimental development version.  The API is still in flux.

The set of implemented features depends on the C<ratchet> switch.

=head1 AUTHORS

The Pugs Team C<< <perl6-compiler@perl.org> >>.

Please join us on irc.freenode.net #perl6 if you'd like to participate.

=head1 SEE ALSO

The Perl 6 Rules Spec: L<http://dev.perl.org/perl6/doc/design/syn/S05.html>

=head1 COPYRIGHT

Copyright 2006 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=cut
