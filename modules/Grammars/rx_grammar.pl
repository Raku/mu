
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

#rule name { <?Perl6.name> }
#rule code { <?Perl6.prog> }
rule name { [<word>|<[\.\:]>]+ }
rule code { ([ <-[{}]> | \{<?code>\} ]* ) }
#rule code { (<-[\}]>+) } # avoid pge bug

rule flag_parsetree   { <':parsetree'> }
rule flag_exhaustive  { <':exhaustive'> | <':ex'> }
rule flag_overlap     { <':overlap'> | <':ov'> }
rule flag_words       { <':words'> | <':w'> }

rule pattern { <term>+ }
rule term { <quantified_term>|<unquantified_term> }
rule unquantified_term {
    <assertion>|<char>
  | <subrule>|<subpattern>|<noncapturing_brackets>
  | <closure>|<literal>
  | <null_term>
}
rule quantified_term { <unquantified_term> <quantifier> }
rule null_term { \< null \> }

rule assertion {
    <assert_bos>
  | <assert_eos>
  | <assert_bol>
  | <assert_eol>
  | <lookahead>
  | <lookbehind>
}
rule assert_bos { \^ }
rule assert_eos { \$ }
rule assert_bol { \^\^ }
rule assert_eol { \$\$ }
rule lookahead  { \< (!)? before \s+ <pattern> \> }
rule lookbehind { \< (!)? after  \s+ <pattern> \> }

rule char { <char_literal>|<char_escaped>|<char_newline> }
rule char_literal { <-[\\\*\+\?\(\)\[\]]> }
rule char_escaped { \\ (<-[tnrfae0xcN]>) }
rule char_newline { \\ n }

rule quantifier {
    <quantifier_opt>
  | <quantifier_rep>
}
rule quantifier_opt { \? | \?\? } # need better name
rule quantifier_rep { \* | \+ | \*\? | \+\? } # need better name

rule subrule                 { \< (\?)? <name> \> }
rule subpattern              { \( <pattern> \) }
rule noncapturing_brackets   { \[ <pattern> \] }
rule closure                 { \{ <code>  \} }
rule literal                 { \<\' ( [ <-<[\\\']>> | \\. ]* ) \'\> }

rule named_scalar_alias         { \$\< <name> \>     \:\= <construct> }
rule numbered_scalar_alias      { \$ $<number>=(\d+) \:\= <construct> }
rule array_alias                { \@\< <name> \>     \:\= <construct> }
rule hash_alias                 { \%\< <name> \>     \:\= <construct> }
rule external_scalar_alias      { \$<name>           \:\= <construct> }
rule external_array_alias       { \@<name>           \:\= <construct> }
rule external_hash_alias        { \%<name>           \:\= <construct> }
rule construct {
    <subrule>
  | <subpattern>
  | <noncapturing_brackets>
  | <quantified_construct>
}
rule quantified_construct { <construct> <quantifier> }

rule comment { \# \N* }
