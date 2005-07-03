
=pod

The world needs a Perl6 regex pattern reference grammar.
Here is a place to accumulate one.

Resources:
  http://search.cpan.org/dist/Perl6-Bible/lib/Perl6/Bible/S05.pod
  http://www.nntp.perl.org/group/perl.perl6.language/20985
  docs/quickref/rules
  http://search.cpan.org/dist/Perl6-Bible/lib/Perl6/Bible/E05.pod
  http://search.cpan.org/dist/Perl6-Bible/lib/Perl6/Bible/A05.pod

=cut

grammar Perl6::Rx;

rule pattern { ... }

rule name { <?Perl6.name> }

#rule code { <?Perl6.prog> }
rule code { ( [ <-[{}]>+ | \{<?code>\} ]* ) }

rule flag_parsetree   { <':parsetree'> }
rule flag_exhaustive  { <':exhaustive'> | <':ex'> }
rule flag_overlap     { <':overlap'> | <':ov'> }
rule flag_words       { <':words'> | <':w'> }

rule subrule                    { \< (\?)? <name> \> }
rule subpattern                 { \( <pattern>    \) }
rule noncapturing_brackets      { \[ <subpattern> \] }
rule closure	                { \{ <code>  \} }
rule literal                    { \<\' ( [ <-<[\\\']>> | \\. ]* ) \'\> }

rule named_scalar_alias		{ \$\< <name> \>     \:\= <construct> }
rule numbered_scalar_alias	{ \$ $<number>=(\d+) \:\= <construct> }
rule array_alias		{ \@\< <name> \>     \:\= <construct> }
rule hash_alias			{ \%\< <name> \>     \:\= <construct> }
rule external_scalar_alias	{ \$<name>           \:\= <construct> }
rule external_array_alias	{ \@<name>           \:\= <construct> }
rule external_hash_alias	{ \%<name>           \:\= <construct> }

rule construct {
    <subpattern>
  | <noncapturing_brackets>
  | <subrule>
  | <quantified_construct>
}

rule quantified_construct { <construct> <quantifier> }

rule quantifier {
    <quantifier_opt>
  | <quantifier_rep>
}
rule quantifier_opt { \? | \?\? } # need better name
rule quantifier_rep { \* | \+ | \*\? | \+\? } # need better name

rule comment { \# \N* \n }

rule bos { \^ }
rule eos { \$ }
rule bol { \^\^ }
rule eol { \$\$ }

rule lookahead  { \< (!)? before \s+ <pattern> \> }
rule lookbehind { \< (!)? after  \s+ <pattern> \> }

rule assertions {
    <bos>
  | <eos>
  | <bol>
  | <eol>
  | <lookahead>
  | <lookbehind>
}

