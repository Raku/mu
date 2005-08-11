
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

rule flag_parsetree   { <':parsetree'> }
rule flag_exhaustive  { <':exhaustive'> | <':ex'> }
rule flag_overlap     { <':overlap'> | <':ov'> }
rule flag_words       { <':words'> | <':w'> }
# XXX - there are more.  Useful short-term. Eventually not needed,
# once macro rxmodinternal:<x> etc works.

#rule name { <?Perl6.name> }
#rule code { <?Perl6.prog> }
rule name { [<word>|<[\.\:]>]+ }
rule code { ([ <-[{}]> | \{ <?code> <'}'> ]* ) }

rule pattern { <term>+? }

rule term { <comment> | <alternation> | <non_alternation> }

rule comment { \# \N* }

rule alternation     { <non_alternation> \| <term>+? }
rule non_alternation { <conjunction> | <non_conjunction> }

rule conjunction     { <non_conjunction> \& <non_alternation>+? }
rule non_conjunction { <repetition> | <alias> | <simple_term> }

rule repetition  { <simple_term> <quantifier> }
rule quantifier  {:w \*\* {<[\d\.]>+}(\??) | <[\?\*\+]>(\??) }

rule alias  { <named_scalar_alias>|<numbered_scalar_alias>|<array_alias>|<hash_alias>|<external_scalar_alias>|<external_array_alias>|<external_hash_alias> }
rule named_scalar_alias     {:w  \$\<<name>\>  \:\=  <construct>  }
rule array_alias            {:w  \@\<<name>\>  \:\=  <construct>  }
rule hash_alias             {:w  \%\<<name>\>  \:\=  <construct>  }
rule numbered_scalar_alias  {:w  \$<number>    \:\=  <construct>  }
rule number  { \d+ }
rule external_scalar_alias  {:w  \$<name>      \:\=  <construct>  }
rule external_array_alias   {:w  \@<name>      \:\=  <construct>  }
rule external_hash_alias    {:w  \%<name>      \:\=  <construct>  }
rule construct  { <construct_but_not_alias>|<alias> }
rule construct_but_not_alias { <subrule>|<capturing_group>|<noncapturing_group>|<quantified_construct> }
rule quantified_construct  {  <construct_but_not_alias> <quantifier>  }

rule simple_term  {
    <metasyntactic_token> | <commit_something>
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

# XXX - not \S, eg :foo('a b'):i:4x 
# must be terminated by () [] :: as in :z()not :z[]not :z::not
rule metasyntactic_token { \:\w\S+ }

rule commit_something { <commit_rule> | <commit_alternation> | <commit_atom> }
rule commit_rule        { \:\:\: }
rule commit_alternation { \:\: }
rule commit_atom        { \: }

rule match_var             { \$ <[\/\.\d]>+ } # XXX - $/<bar> etc
rule psuedo_scalar_literal { \$ <name> } # $foo ==> <'$foo'>  Scary.
rule interpolated_array    { \@ <name> <element_expr>? }
rule interpolated_hash     { \% <name> <element_expr>? }
# XXX - kludge
rule element_expr { [ \[ <-[\]]> \] | \{ <-[\}]> <'}'> ] <element_expr>? }

rule null_term  { \< null \> }

rule assertion  { <assert_bos>|<assert_eos>|<assert_bol>|<assert_eol>|<lookahead>|<lookbehind>|<word_boundary> }
rule assert_bos  { \^ }
rule assert_eos  { \$ }
rule assert_bol  { \^\^ }
rule assert_eol  { \$\$ }
rule lookahead   { \< (!?) before <ws> <pattern> \> }
rule lookbehind  { \< (!?) after  <ws> <pattern> \> }
rule word_boundary  { \\ b }
#rule element_boundary { \< , \> } # punt for now

rule subrule                { ([\<\<|\<|\x[abd]]) (\??) <name> [ \( <argument_list> \) | <pattern> | <null> ] [\>\>|\>|\x[bbd]] }
rule argument_list { [ <-[\\\(\)]> | \\ . | \( <argument_list> \) ]* }
rule capturing_group        { \( <pattern> \) }
rule noncapturing_group     { \[ <pattern> \] }
rule closure                { \{ <code> <'}'> }
rule literal                { \<(!?)\' [ <-[\\\']> | \\ . ]* \'\> }
rule interpolated_literal   { \<(!?)\" [ <-[\\\"]> | \\ \" ]* \"\> }
rule code_rule              { \<(!?)\{ <code> \}\> }
rule code_assertion         { \<(!?)\( <code> \)\> }
rule code { [ <-[\{\}]> | \{ <code> <'}'> ]* }
rule metasyntactic_var      { \<(!?)<[\$\@\%]> <-[\>]>+ \> | <metasyntactic_sub> }
rule metasyntactic_sub      { \<(!?)<[&]> <ident> [ \( <argument_list> \) ]? \> }
rule symbolic_indirect_rule { \<(!?)\:\:\( <code> \)\> }

rule character  { <a_dot>|<logical_grapheme>|<abbreviated_class>|<explicit_class>|<escape_sequence>|<named_character>|<simple_character> }
rule a_dot  { \. }
rule logical_grapheme { \< \. \> }
rule abbreviated_class  { \\ <[dswDSWhvnHVN]> }
rule explicit_class  { \< <character_set>+ \> }
rule character_set  { (\+|-)? \[ [ <-[\\\]]> | \\ . ]* \] | (\+|-) <subrule> }

rule escape_sequence { <simple_escape_sequence> | <octal_escape_sequence> | <hexidecimal_escape_sequence> }
rule simple_escape_sequence { \\ <[trfeTRFE]> }
rule octal_escape_sequence  {
    \\ 0
      [ <octaldigit><octaldigit><octaldigit>
      | <bracket_open> [<octaldigit> | ; ]+ <bracket_close>
      ] }
rule hexidecimal_escape_sequence {
    \\ <[xX]>
      [ <hexdigit><hexdigit><hexdigit><hexdigit>
      | <hexdigit><hexdigit>
      | <bracket_open> [<hexdigit> | ; ]+ <bracket_close>
      ] }
rule bracket_open  { <[\{\(\[\<\>\/\!\=\?\#\x[abd]\x[bbd]]> }
rule bracket_close { <[\}\)\]\>\<\/\!\=\?\#\x[bbd]\x[abd]]> }

rule named_character  { \\ <[cC]> \[ <-[\]]>+ \] }

# XXX - PGE doesnt do <-<metacharacter>> yet.
rule simple_character  { <-[\{\}\[\]\(\)\^\$\.\|\*\+\?\#\\]> }
rule metacharacter { <[\{\}\[\]\(\)\^\$\.\|\*\+\?\#\\]> }
