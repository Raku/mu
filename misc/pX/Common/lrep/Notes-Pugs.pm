package Pugs;





1;

=head1 The User Story

Say we have a Perl 6 snippet here:

    my $x = 'World';
    say "Hello, $x!";

Pugs::Parser gets it, turns it into a AST node (the API is
specified in Perl6::API::AST - so maybe the implementation
lives there too?)

Pugs::Compiler (or Analyzer?) gets it, emit a Module object
that defines ::Main.  A Module object mixes in both Package
and Code -- because it is both a namespace of symbols and has
a main body closure. 

the Package part of the Module is simply a hash from strings
(symbol names) to objects (containers)

the Code part consists of a Pad, a Sig and a Body.

the Pad contains My/State/Temp markers for container
allocation, as well as the compile-time-bound values for them.
It includes the parameters as well as anything declared within
that scope.

the Sig contains a simply structure of the signature; it does
not allocate containers.

the Body is just a structure with three node forms: Var
(variable lookup), App (application) and Lit (literal).

App is divided to Multiple, Single and Primitive dispatch.
Lit is divided to various intrinsic object types, including
intrinsic Code (which then has Pad/Sig/Body too)
Var is either a static lookup (lex only) or a dynamic lookup. 

This object-serialization format is shared across all runtimes.

=head1 Directory Layout

lib/Pugs/Compiler.pm  # functional things
lib/Perl6/API/Code.pm # userland objects -- those are written in p6 perhaps?

I propose: no lib6/ and lib/ distinctions -- .pm files should be both
p5 and p6 capable; p5 ones begin with

    package Foo;

and p6 ones begin with:

    use Perl-6.0;
    class Foo;

and we can stepwise replace p5 ones with p6 ones _without_ changings its name.

of course that has the problem of supplying Perl.pm itself - but I think to
do it Right As Specified is more important.


Pugs::Compiler::{name} ? for simultaneous versions

I'm not sure... I think Pugs::Compiler could work as the
basic interface, and if you'd like to offer more, subclass
it and call it Pugs::Compiler::Foo.

use Pugs::Compiler-fglock;
# this would load Pugs::Compiler::fglock as well as 
# all ::fglock things -- that is, Pugs::Compiler::SomeComponent::fglock
# as well -- and "use Pugs-fglock" will load all ::fglock things
# for Pugs::Compiler, Pugs::Emitter, etc?


I think subclassing is overrated, yeah -- mixins are much better
so lib/Pugs/Compiler.pm
but augmentations lives in somewhere else?
libX/fglock/Pugs/Compiler.pm?
lib/Pugs/Compiler.fglock.pm?

use Pugs-fglock; # everything is fglock
use Pugs qw( Parser fglock );

but where does the .pm live for real? I like per-user dirs

lib/fglock/Compiler.pm
lib/Pugs/Compiler.pm

I think that works!

ok!


how about:



... new( lib => ..., parser => fglock ... )



=head1 Compiler processes

=head2 Grammar engine

* Pugs::Grammar::Rule

  # Domain-specific-language-like syntax, contained in a package
  { package Pugs::Grammar::Rule;
      rule ( alternation ( literal ( 'a' ), .... )
  }

Pugs::Grammar::Rule is a functional-style module.

The rule() function provides an interface between the low-level functional
operators, and the higher level OO/Overload runtime. 

Rule functions share a common API, which permits the intercommunication of data 
about: backtracking, aborting, capturing.

A rule function gets as argument a list:

:(Str $match_against, :$next_state, Bool :$capture?
  --> Pugs::Grammar::Match)

  $match_against - a string to match
  $next_state    - an optional "continuation"
  $capture       - capture then entire match. For example:
  
  # returns a Match object that has the 'capture' field set to 'abc'
  match_word( 'abc123', :capture );

  # returns a Match object that has no capture (but the 'bool' field is still true)
  match_word( 'abc123' );

A rule function returns:

    undef - match failed    # spec says it returns normal data!
-- should I remove this ^^^^ from the code
because

shouldn't matchfail still be returning something?
maybe just overload bool to false ;)
(I mean, seriously, because that's what p6 does)
and also you can overload @{} %{} &{} + ~

I think it should be magical at "unboxed" level yes.
I mean, if I use Pugs::Grammar in p5 code
I'd _expect_ the match object returned to carry the same magic.

overload in p5 is just static %OVERLOAD entry
the only overhead is a SVMG (i.e. bless())
you can return the same failure object always
then there's absolutely no overhead
I'm not sure about this - failed matche's .end is undefined



How about:

  rule(
      alternation(
          literal( .... )
         )
  )

- match is the "boxing" thing - things below it are unboxed!

I'm fine if you can pull that off :-)
(it's a great API idea)
cool, it's a deal then
great - because it permits all optimizations to stay!
:)


not sure - the match object includes the string position, etc
even if it fails?

ok - I'm just afraid that even a simple match involves so many submatches that this thing will get real slow


Yes - but isn't it magical, at the "unboxed" level?

yes - it WAS another optimization ...

or a Pugs::Grammar::Match object containing:

    state - a "continuation" or undef
    bool - an "assertion" (true/false)
    match - the "match" tree or undef
    tail - the string tail or undef
    capture - the tree of captured things
    abort - the match was stopped by a { return } or a fail(),
           and it should not backtrack or whatever

Continuations are used for backtracking.

A "ruleop" function gets some arguments and returns a "rule".



=cut
