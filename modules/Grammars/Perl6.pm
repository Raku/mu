=pod

Perl6's grammer written as a Perl6 rules.  A prerequisite for self hosting.

=cut

grammar Perl6;

# Numbers

# Numbers base 10.

rule natural { <digit> [_* <digit> _*]* };

rule Int {[\+|\-]? <natural>};

rule decimal { <Int> [\. <natural>? ]?
              | [\+|\-]? \. <natural>
             };

rule Rat  { <decimal> [ [e|E] <Int>]? };

# Numbers base anything else.

rule binary { 0b <[01]>+ };

rule hex    { 0x <[0..9a..fA..F]>+};

rule oct    { 0o <[0..7]>+};

# A rule to match any perl style number.

rule Num { Int | Rat | binary | hex | oct  };







