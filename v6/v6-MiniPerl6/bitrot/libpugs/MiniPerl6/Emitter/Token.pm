# Do not edit this file - Generated by MiniPerl6
use v6-alpha;
class Rul { sub constant { my $List__ = \@_; my $str; do {  $str = @_.[0]; [$str] }; my  $len = $str.chars(); do { if (($str eq '\\')) { $str = '\\\\' } else {  } }; do { if (($str eq '\'')) { $str = '\\\'' } else {  } }; do { if ($len) { ('( ( \'' ~ ($str ~ ('\' eq substr( $str, $MATCH.to, ' ~ ($len ~ (')) ' ~ ('  ?? (1 + $MATCH.to( ' ~ ($len ~ (' + $MATCH.to ))' ~ ('  !! (0) ' ~ ')'))))))))) } else { return('1') } } }} 
;
class Rul::Quantifier { has $.term; has $.quant; has $.greedy; has $.ws1; has $.ws2; has $.ws3; method emit { do { [] }; $.term.emit() }} 
;
class Rul::Or { has $.or; method emit { do { [] }; ('do { ' ~ ('my $pos1 := $MATCH.to(); do{ ' ~ (@.or>>.emit().join('} || do { $MATCH.to( $pos1 ); ') ~ '} }'))) }} 
;
class Rul::Concat { has $.concat; method emit { do { [] }; ('(' ~ (@.concat>>.emit().join(' && ') ~ ')')) }} 
;
class Rul::Subrule { has $.metasyntax; method emit { do { [] }; my  $meth = ((1 + index($.metasyntax, '.')) ?? $.metasyntax !! ('$grammar.' ~ $.metasyntax)); ('do { ' ~ ('my $m2 := ' ~ ($meth ~ ('($str, $MATCH.to); ' ~ ('if $m2 { $MATCH.to( $m2.to ); $MATCH{\'' ~ ($.metasyntax ~ ('\'} := $m2; 1 } else { 0 } ' ~ '}'))))))) }} 
;
class Rul::SubruleNoCapture { has $.metasyntax; method emit { do { [] }; my  $meth = ((1 + index($.metasyntax, '.')) ?? $.metasyntax !! ('$grammar.' ~ $.metasyntax)); ('do { ' ~ ('my $m2 := ' ~ ($meth ~ ('($str, $MATCH.to); ' ~ ('if $m2 { $MATCH.to( $m2.to ); 1 } else { 0 } ' ~ '}'))))) }} 
;
class Rul::Var { has $.sigil; has $.twigil; has $.name; method emit { do { [] }; my  $table = { '$' => '$','@' => '$List_','%' => '$Hash_','&' => '$Code_', }; ($table.{$.sigil} ~ $.name) }} 
;
class Rul::Constant { has $.constant; method emit { do { [] }; my  $str = $.constant; Rul::constant($str) }} 
;
class Rul::Dot { method emit { do { [] }; ('( (\'\' ne substr( $str, $MATCH.to, 1 )) ' ~ ('  ?? (1 + $MATCH.to( 1 + $MATCH.to ))' ~ ('  !! (0) ' ~ ')'))) }} 
;
class Rul::SpecialChar { has $.char; method emit { do { [] }; my  $char = $.char; do { if (($char eq 'n')) { my  $rul = Rul::SubruleNoCapture.new( 'metasyntax' => 'newline', );$rul = $rul.emit();return($rul) } else {  } }; do { if (($char eq 'N')) { my  $rul = Rul::SubruleNoCapture.new( 'metasyntax' => 'not_newline', );$rul = $rul.emit();return($rul) } else {  } }; do { if (($char eq 'd')) { my  $rul = Rul::SubruleNoCapture.new( 'metasyntax' => 'digit', );$rul = $rul.emit();return($rul) } else {  } }; do { if (($char eq 's')) { my  $rul = Rul::SubruleNoCapture.new( 'metasyntax' => 'space', );$rul = $rul.emit();return($rul) } else {  } }; return(Rul::constant($char)) }} 
;
class Rul::Block { has $.closure; method emit { do { [] }; ('do { ' ~ ('my $ret := ( sub {' ~ ('do {' ~ ($.closure ~ ('}; ' ~ ('\'974^213\' } ).();' ~ ('if $ret ne \'974^213\' {' ~ ('$MATCH.capture( $ret ); ' ~ ('return $MATCH;' ~ ('};' ~ ('1' ~ '}'))))))))))) }} 
;
class Rul::InterpolateVar { has $.var; method emit { do { [] }; say(('# TODO: interpolate var ' ~ ($.var.emit() ~ ''))); die() }} 
;
class Rul::NamedCapture { has $.rule; has $.ident; method emit { do { [] }; say(('# TODO: named capture ' ~ ($.ident ~ (' := ' ~ ($.rule.emit() ~ ''))))); die() }} 
;
class Rul::Before { has $.rule; method emit { do { [] }; ('do { ' ~ ('my $tmp := $MATCH; ' ~ ('$MATCH := ::MiniPerl6::Perl5::Match( \'str\' => $str, \'from\' => $tmp.to, \'to\' => $tmp.to, \'bool\' => 1  ); ' ~ ('$MATCH.bool( ' ~ ($.rule.emit() ~ ('); ' ~ ('$tmp.bool( ?$MATCH ); ' ~ ('$MATCH := $tmp; ' ~ ('?$MATCH; ' ~ '}'))))))))) }} 
;
class Rul::NotBefore { has $.rule; method emit { do { [] }; ('do { ' ~ ('my $tmp := $MATCH; ' ~ ('$MATCH := ::MiniPerl6::Perl5::Match( \'str\' => $str, \'from\' => $tmp.to, \'to\' => $tmp.to, \'bool\' => 1  ); ' ~ ('$MATCH.bool( ' ~ ($.rule.emit() ~ ('); ' ~ ('$tmp.bool( !$MATCH ); ' ~ ('$MATCH := $tmp; ' ~ ('?$MATCH; ' ~ '}'))))))))) }} 
;
class Rul::NegateCharClass { has $.chars; method emit { do { [] }; say('TODO NegateCharClass'); die() }} 
;
class Rul::CharClass { has $.chars; method emit { do { [] }; say('TODO CharClass'); die() }} 
;
class Rul::Capture { has $.rule; method emit { do { [] }; say('TODO RulCapture'); die() }} 
;