=pod

Perl 6's grammar written as a Perl 6 regexs.  A prerequisite for self hosting.

=cut

use v6-alpha;

grammar Perl6::Grammar-0.0.1;

# Top level structures

token code { <-[{}]>* } # a section of code

#regex block { \{ <code> \} } # a block of code
# pugs parsing problem? - 2005 Jul 31
token block { <[{]> <code> <[}]> } # a block of code

# subs and sub-like structures

rule lexicalsub { # from A06
        <lexscope> <type>?
        <submodifer>? <subintro> <subname> <psignature>?
        <trait>*
        <block>
}

rule lexicalsub { # from A06
        <lexscope> <type>?
        <submodifer>? <subintro> <subname> <psignature>?
        <trait>*
        <block>
}
rule packagesub { # from A06
        <submodifer> <subintro> <subname> <psignature>?
        <trait>*
        <block>
}

rule anonsub {
        <subintro> <psignature>?
        <trait>*
        <block>
}

# Pointy subs

rule pointysub { # from A06
        -\> <signature> <block>
}

# Variables 
# From A06

regex sigil { <[$@%&]> <[*.?^]>? }   # "What is that, swearing?"
regex variable { <sigil> <nameZZ> [ \( <siglet> \) ]? }

# Bare subs
# from A06

#rule baresub {
#        <block> { .find_placeholders() }
#}

# Identifiers

regex name_sec { <-[0..9:.]> <-[:]>* };

regex nameZZ {  <name_sec>
             | [\:\: <name_sec> ]+
             | <name_sec> [\:\: <name_sec> ]+};

## What is the difference between an ident, a name and a subname
## A06 doesn't tell me even though it uses them.  These are guesses
## until we are enlightened.

regex subname { \*? <nameZZ> };

regex identZZ { <nameZZ> };

# Numbers

# base 10

regex natural { <digit> [_* <digit> _*]* };

regex Int {[\+|\-]? <natural>};

regex decimal { <Int> [\. <natural>? ]?
              | [\+|\-]? \. <natural>
             };

regex Rat  { <decimal> [ [e|E] <Int>]? };

# base N

regex binary { 0b <[01]>+ };

regex hex    { 0x <[0..9a..fA..F]>+};

regex oct    { 0o <[0..7]>+};

# any perl style number

regex Num { Int | Rat | binary | hex | oct  };

# these are defined in A06

# Siglets

rule siglet {
        [<paramlet> [<[,:]> <paramlet> ]* ]?
}

#rule paramlet {
#        [ <type> <zone>? <varlet>? <trait>*     # require type
#        | <zone> <varlet>? <trait>*             # or zone
#        | <varlet> <trait>*                     # or varlet
#        | \[ <siglet> \]        # treat single array ref as an arg list
#        ]
#}
# comments appear to be poisonous - 2005 Jul 31
rule paramlet {
        [ <type> <zone>? <varlet>? <trait>*
        | <zone> <varlet>? <trait>*
        | <varlet> <trait>*
        | \[ <siglet> \]
        ]
}

rule varlet {
        <sigil> [ \( <siglet> \) ]?
}

# Defaults

rule defval { \= <item> }

# Placeholders

regex placeholder { <sigil> \^ <identZZ> }

# Formal parameter syntax

#rule parameter {
#        [ <type>? <zone>? <variable> <trait>* <defval>?
#        | \[ <signature> \]     # treat single array ref as an arg list
#        ]
#}
# comments appear to be poisonous - 2005 Jul 31
rule parameter {
         [ <type>? <zone>? <variable> <trait>* <defval>?
         | \[ <signature> \]
         ]
}


regex type { yada type }
regex zone { yada zone }

rule signature {
        [<parameter> [<[,:]> <parameter> ]* ]?
}

# The sub form

regex subintro { sub | method | submethod | regex | macro }

regex lexscope { my | our }

regex submodifer { multi }

rule psignature { \( <signature> \) }

rule psiglet { \( <siglet> \) }

rule scopedsubvar {
        <lexscope> <type>? &<subname> <psiglet>? <trait>*
}

rule unscopedsubvar {
        &<subname> <psiglet>? <trait>*
}

rule trait {
          is <identZZ>[\( <traitparam> \)]?
        | will <identZZ> <closure>
        | of <type>
        | returns <type>
}

regex traitparam { yada traitparam }
regex closure { yada closure }

