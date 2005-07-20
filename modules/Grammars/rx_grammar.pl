
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

rule term { <alternation> | <non_alternation> }

rule alternation     { <non_alternation> \| <term>+ }
rule non_alternation { <conjunction> | <non_conjunction> }

rule conjunction     { <non_conjunction> \& <non_alternation>+ }
rule non_conjunction { <repetition> | <alias> | <simple_term> }

rule repetition  { <simple_term> <quantifier> }
rule quantifier  { \*\*{<[\d\.]>+}(\??) | <[\?\*\+]>(\??) }

rule alias  { <named_scalar_alias>|<numbered_scalar_alias>|<array_alias>|<hash_alias>|<external_scalar_alias>|<external_array_alias>|<external_hash_alias> }
rule named_scalar_alias     {  \$\< <name> \> \:\= <construct>  }
rule array_alias            {  \@\< <name> \> \:\= <construct>  }
rule hash_alias             {  \%\< <name> \> \:\= <construct>  }
rule numbered_scalar_alias  {  \$  <number>   \:\= <construct>  }
rule number  { \d+ }
rule external_scalar_alias  {  \$<name>       \:\= <construct>  }
rule external_array_alias   {  \@<name>       \:\= <construct>  }
rule external_hash_alias    {  \%<name>       \:\= <construct>  }
rule construct  { <subrule>|<capturing_group>|<noncapturing_group>|<quantified_construct> }
rule quantified_construct  {  <construct> <quantifier>  }

rule simple_term  {
    <metasyntactic_token>
  | <match_var>
  | <psuedo_scalar_literal>
  | <interpolated_array> | <interpolated_hash>
  | <null_term>
  | <assertion>
  | <subrule>
  | <capturing_group> | <noncapturing_group>
  | <literal> | <interpolated_literal>
  | <closure> | <code_rule> | <code_assertion>
  | <metasyntactic_var> | <symbolic_indirect_rule>
  | <character>
}

rule metasyntactic_token { \: \S+ } # XXX - eg :foo('a b')

rule match_var             { \$ <[\/\.\d]>+ }
rule psuedo_scalar_literal { \$ <name> } # $foo ==> <'$foo'> # XXX - scary!
rule interpolated_array    { \@ <name> }
rule interpolated_hash     { \% <name> }

rule null_term  { \< null \> }

rule assertion  {
    <anchor_bos> | <anchor_eos> | <anchor_bol> | <anchor_eol>
  | <lookahead> | <lookbehind>
  | <word_boundary>
}
rule anchor_bos  { \^ }
rule anchor_eos  { \$ }
rule anchor_bol  { \^\^ }
rule anchor_eol  { \$\$ }
rule lookahead   { \< (!?) before <ws> <pattern> \> }
rule lookbehind  { \< (!?) after  <ws> <pattern> \> }
rule word_boundary  { \\ b }

rule subrule                { \< (\??) <name> \> }
rule capturing_group        { \( <pattern> \) }
rule noncapturing_group     { \[ <pattern> \] }
rule closure                { \{ <code> <'}'> }
rule literal                { \<(!?)\' [ <-[\\\']> | \\ . ]* \'\> }
rule interpolated_literal   { \<(!?)\" [ <-[\\\"]> | \\ \" ]* \"\> }
rule code_rule              { \<(!?)\{ <code> \}\> }
rule code_assertion         { \<(!?)\( <code> \)\> }
rule code { [ <-[\{\}]> | \{ <code> <'}'> ]* }
rule metasyntactic_var      { \<(!?)<[\$\@\%\&]> <-[\>]>+ \> }
rule symbolic_indirect_rule { \<(!?)\:\:\( <code> \)\> }

rule character  { <character_class>|<escape_sequence>|<simple_character> }
rule character_class  { <a_dot>|<logical_grapheme>|<abbreviated_class>|<explicit_class> }
rule a_dot  { \. }
rule logical_grapheme { \< \. \> }
rule abbreviated_class  { \\ <[dswDSWhvnHVN]> }
rule explicit_class  { \< <character_set>+ \> }
rule character_set  { (\+|-)? \[ [ <-[\\\]]> | \\ . ]* \] | (\+|-) <subrule> }

rule escape_sequence  { \\ <[trfeTRFE]> }

rule simple_character  { <-[\{\}\[\]\(\)\^\$\.\|\*\+\?\#\\]> } # <-<metacharacter>>
rule metacharacter { <[\{\}\[\]\(\)\^\$\.\|\*\+\?\#\\]> }

rule comment { \# \N* }
