package Pugs;







1;





### There is too much in this file 

### separate into:

- Introduction

- Documentation Index

- Compiler internals:

    Compiler

    Compiler::Perl6

    Compiler::Rule

    Compiler::YAML

    Compiler::Perl5

    Compiler::Parrot

- AST, Grammar, Optimizer internals:

    AST::Perl6::API

    Grammar::Perl6

    Optimizer::AST   -- optimizes an AST

    Grammar::MOP     -- implements OO using macros

    

=head1 Definitions



### change to kwid? - easier to indent (but not supported by p5 by default)

### move definitions to end-of-file?



* boxed / unboxed



* AST Abstract syntax tree

  - the data structure that the Parser generates

  (link to wikipedia?)



* DSL Domain-specific-language



...



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



### - see "Modifying the compiler" below 



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



### how about lib/Pugs-fglock/Compiler.pm

###   just like use Pugs-fglock above



=head1 Modifying the compiler



Let's say you have a new Grammar and Emitter for your own language.



  # assuming:

  # - your target system runs Perl5

  # - you have loaded the Grammar file to $new_parser

  # - and the emitter to $new_emitter

  

  # build a "normal" compiler that has: 

  # - a Perl6 Grammar with the Meta Object Protocol (implemented as macros), 

  #   Primitives,

  # - an optimizer chain,

  # - the default Perl6 libs,

  # - an emitter that generates perl5

  # load it as 'the' handler for 'eval( ... , :lang<perl6> )

  my $compiler = Pugs::Compiler->new( 

      lang      => 'perl6',

      parser    => ['Perl6', 'MOP6', 'PrimP6']

      optimizer => ['Op1' 'Op2'],

      lib       => ['/lib' ],

      emitter   => 'Perl5' );



  # same thing, but reads lib/Pugs/Compiler/Language/Perl6.yaml

  # this is the command used to implement 'eval( ... , :lang<name> )'

  my $compiler = Pugs::Compiler->new( 

      lang => 'Perl6' );



my $new_parser_p5 = $compiler->( $new_parser );

  

  # build a new compiler, that uses:

  # - the new Grammar

  # - no optimizer

  # - the new emitter

  my $compiler_OO_AST = Pugs::Compiler->load( 

      parser  => ['NewLang'],

      optimizer => [],

      emitter => \$new_emitter_p5 );

      

=head2 Example: a YAML compiler

      

* load the YAML compiler



  *compile::YAML = Pugs::Compiler->new( 

      lang      => 'YAML',

      parser    => ['YAML']

      optimizer => [],

      lib       => ['/lib' ],

      emitter   => 'YAML' );



  # use the configuration file (Pugs::Compiler::YAML.pm)

  *compile::YAML = Pugs::Compiler->new( lang => 'YAML' );



* use the compiler



  my $data = compile::eval( $source, lang => 'YAML' );



* Files



  /lib/Pugs/Compiler/YAML.pm  # configuration (like CPAN/Config.pm)

  /lib/Pugs/Grammar/YAML.pm   # main Grammar file

  /lib/Pugs/Grammar/YAML/*    # Grammar related files

  /lib/Pugs/Emitter/YAML.pm   # main Emitter file

  /lib/Pugs/Emitter/YAML/*    # Emitter related files



=head1 Compiler processes



### separate "processes" from actual module documentation?



=head2 Grammar engine



* Pugs::Grammar::Rule and Pugs::Grammar::Match



Rule constructors return Rule objects, which are "matcher" things.

Rules operate on strings (normally) and return Match objects.



### examples will be in p5 or p6?



  # invoking the rule compiler

### Rule::compile or Rule->compile? (assuming example in p5)

  my $matcher = Pugs::Grammar::Rule::compile ( ".*", options... );

  my $match = $matcher( "hello" );



### "internals" thing - should be documented only in Pugs::Grammar::Rule 

  # low level operators

  # Domain-specific-language-like syntax, contained in a package

### rule must ever use Rule::rule form - never Rule->rule ??? (assuming example in p5)

  my $matcher;   # must be declared outside of the block

  { package Pugs::Grammar::Rule;

      $matcher = rule ( alternation ( literal ( 'a' ), .... );

  }



Match objects (Pugs::Grammar::Match) contain:



### Match actually contain getters for $() and such

### getters need to know what you want (str, bool, hash, array, match)



- compile()



### obvious - not document ?

...



- rule()



### internals - move to module documentation



Rules, just like other Perl 6 values, exists in 'boxed' 

and 'unboxed' versions. 



The rule() constructor provides an interface between the 

low-level functional operators, and the higher level OO/Overload 

runtime. 



- internal rule operators



Internal rule operators share a common API, which permits the intercommunication 

of data about: backtracking, aborting, capturing.



There are 2 kinds of ruleop operators:



- higher order operators: alternation, concatenation.

- "plain" match operators: like 'literal'.



Higher order operators take a list of match operators, and return a new 

match operator.



A ruleop() function gets as argument a list:



### we don't need a "high" level interface (named args) 

###    because the actual rule object will be full OO



:(Str $match_against, :$next_state, Bool :$capture?

  --> Pugs::Grammar::Match)



  $match_against - a string to match

  $next_state    - an optional "continuation"

  $capture       - capture then entire match. For example:

  

  # returns a Match object that has the 'capture' field set to 'abc'

  match_word( 'abc123', :capture );



  # returns a Match object that has no capture (but the 'bool' field is still true)

  match_word( 'abc123' );



A ruleop function returns:



    # spec says it returns normal data, but this is a low-level op...

    undef - if match failed    



    state - a "next-state" or undef

    bool - an "assertion" (true/false)

    match - the "match" tree or undef

    tail - the string tail or undef

    capture - the tree of captured things

    abort - the match was stopped by a { return } or a fail(),

           and it should not backtrack or whatever

                            

=for notes about fail()

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

=cut



### current implementation is always anchored at ^

### because it was specialized for subrules

### - needs to be generalized, and the anchor should only

###   work when needed (or if there is a switch telling so)

