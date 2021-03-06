{ package KindaPerl6::Grammar; 
# Do not edit this file - Perl 5 generated by KindaPerl6
# AUTHORS, COPYRIGHT: Please look at the source file.
use v5;
use strict;
no strict "vars";
use constant KP6_DISABLE_INSECURE_CODE => 0;
use KindaPerl6::Runtime::Perl5::Runtime;
my $_MODIFIED; INIT { $_MODIFIED = {} }
INIT { $_ = ::DISPATCH($::Scalar, "new", { modified => $_MODIFIED, name => "$_" } ); }
do {do { if (::DISPATCH(::DISPATCH(::DISPATCH(  ( $GLOBAL::Code_VAR_defined = $GLOBAL::Code_VAR_defined || ::DISPATCH( $::Routine, "new", )  ) 
, 'APPLY', $::KindaPerl6::Grammar )
,"true"),"p5landish") ) { do {} }  else { do {do {::MODIFIED($::KindaPerl6::Grammar);
$::KindaPerl6::Grammar = ::DISPATCH( ::DISPATCH( $::Class, 'new', ::DISPATCH( $::Str, 'new', 'KindaPerl6::Grammar' )
 )
, 'PROTOTYPE',  )
}} } }
;     sub _rule_space {
    local $GLOBAL::_M = [ $GLOBAL::_M, 'create', pos(), \$_ ]; 
    $GLOBAL::_M2 = $GLOBAL::_M; /\G[[:space:]]/g && do { $GLOBAL::_M = [ $GLOBAL::_M, 'to', pos() ]; 
    $GLOBAL::_M2 = $GLOBAL::_M }; } 
::DISPATCH(::DISPATCH($::KindaPerl6::Grammar,"HOW"),"add_method", ::DISPATCH( $::Str, "new", "space" ), ::DISPATCH( $::Method, "new", { code => sub { local $GLOBAL::_Class = shift; undef $GLOBAL::_M2; my ($str,$pos) = @_;$str = defined($str) ? $str : $_;local $_ = ( ref($str) ? ::DISPATCH( $str, "Str" )->{_value} : $str ); pos($_) = $pos->{_value} if ref $pos;if ( _rule_space() ) { Match::from_global_data( $GLOBAL::_M2 ); $MATCH = $GLOBAL::MATCH = pop @Match::Matches; } else { $MATCH = $GLOBAL::MATCH = Match->new(); } @Match::Matches = (); return $MATCH; } } ), );     sub _rule_word {
    local $GLOBAL::_M = [ $GLOBAL::_M, 'create', pos(), \$_ ]; 
    $GLOBAL::_M2 = $GLOBAL::_M; /\G[[:word:]]/g && do { $GLOBAL::_M = [ $GLOBAL::_M, 'to', pos() ]; 
    $GLOBAL::_M2 = $GLOBAL::_M }; } 
::DISPATCH(::DISPATCH($::KindaPerl6::Grammar,"HOW"),"add_method", ::DISPATCH( $::Str, "new", "word" ), ::DISPATCH( $::Method, "new", { code => sub { local $GLOBAL::_Class = shift; undef $GLOBAL::_M2; my ($str,$pos) = @_;$str = defined($str) ? $str : $_;local $_ = ( ref($str) ? ::DISPATCH( $str, "Str" )->{_value} : $str ); pos($_) = $pos->{_value} if ref $pos;if ( _rule_word() ) { Match::from_global_data( $GLOBAL::_M2 ); $MATCH = $GLOBAL::MATCH = pop @Match::Matches; } else { $MATCH = $GLOBAL::MATCH = Match->new(); } @Match::Matches = (); return $MATCH; } } ), );     sub _rule_digit {
    local $GLOBAL::_M = [ $GLOBAL::_M, 'create', pos(), \$_ ]; 
    $GLOBAL::_M2 = $GLOBAL::_M; /\G[[:digit:]]/g && do { $GLOBAL::_M = [ $GLOBAL::_M, 'to', pos() ]; 
    $GLOBAL::_M2 = $GLOBAL::_M }; } 
::DISPATCH(::DISPATCH($::KindaPerl6::Grammar,"HOW"),"add_method", ::DISPATCH( $::Str, "new", "digit" ), ::DISPATCH( $::Method, "new", { code => sub { local $GLOBAL::_Class = shift; undef $GLOBAL::_M2; my ($str,$pos) = @_;$str = defined($str) ? $str : $_;local $_ = ( ref($str) ? ::DISPATCH( $str, "Str" )->{_value} : $str ); pos($_) = $pos->{_value} if ref $pos;if ( _rule_digit() ) { Match::from_global_data( $GLOBAL::_M2 ); $MATCH = $GLOBAL::MATCH = pop @Match::Matches; } else { $MATCH = $GLOBAL::MATCH = Match->new(); } @Match::Matches = (); return $MATCH; } } ), );     sub _rule_backslash {
    local $GLOBAL::_M = [ $GLOBAL::_M, 'create', pos(), \$_ ]; 
    $GLOBAL::_M2 = $GLOBAL::_M; /\G\\/g && do { $GLOBAL::_M = [ $GLOBAL::_M, 'to', pos() ]; 
    $GLOBAL::_M2 = $GLOBAL::_M }; } 
::DISPATCH(::DISPATCH($::KindaPerl6::Grammar,"HOW"),"add_method", ::DISPATCH( $::Str, "new", "backslash" ), ::DISPATCH( $::Method, "new", { code => sub { local $GLOBAL::_Class = shift; undef $GLOBAL::_M2; my ($str,$pos) = @_;$str = defined($str) ? $str : $_;local $_ = ( ref($str) ? ::DISPATCH( $str, "Str" )->{_value} : $str ); pos($_) = $pos->{_value} if ref $pos;if ( _rule_backslash() ) { Match::from_global_data( $GLOBAL::_M2 ); $MATCH = $GLOBAL::MATCH = pop @Match::Matches; } else { $MATCH = $GLOBAL::MATCH = Match->new(); } @Match::Matches = (); return $MATCH; } } ), );     sub _rule_newline {
    local $GLOBAL::_M = [ $GLOBAL::_M, 'create', pos(), \$_ ]; 
    $GLOBAL::_M2 = $GLOBAL::_M; /\G(?m)(\n\r?|\r\n?)/g && do { $GLOBAL::_M = [ $GLOBAL::_M, 'to', pos() ]; 
    $GLOBAL::_M2 = $GLOBAL::_M }; } 
::DISPATCH(::DISPATCH($::KindaPerl6::Grammar,"HOW"),"add_method", ::DISPATCH( $::Str, "new", "newline" ), ::DISPATCH( $::Method, "new", { code => sub { local $GLOBAL::_Class = shift; undef $GLOBAL::_M2; my ($str,$pos) = @_;$str = defined($str) ? $str : $_;local $_ = ( ref($str) ? ::DISPATCH( $str, "Str" )->{_value} : $str ); pos($_) = $pos->{_value} if ref $pos;if ( _rule_newline() ) { Match::from_global_data( $GLOBAL::_M2 ); $MATCH = $GLOBAL::MATCH = pop @Match::Matches; } else { $MATCH = $GLOBAL::MATCH = Match->new(); } @Match::Matches = (); return $MATCH; } } ), );     sub _rule_not_newline {
    local $GLOBAL::_M = [ $GLOBAL::_M, 'create', pos(), \$_ ]; 
    $GLOBAL::_M2 = $GLOBAL::_M; /\G./g && do { $GLOBAL::_M = [ $GLOBAL::_M, 'to', pos() ]; 
    $GLOBAL::_M2 = $GLOBAL::_M }; } 
::DISPATCH(::DISPATCH($::KindaPerl6::Grammar,"HOW"),"add_method", ::DISPATCH( $::Str, "new", "not_newline" ), ::DISPATCH( $::Method, "new", { code => sub { local $GLOBAL::_Class = shift; undef $GLOBAL::_M2; my ($str,$pos) = @_;$str = defined($str) ? $str : $_;local $_ = ( ref($str) ? ::DISPATCH( $str, "Str" )->{_value} : $str ); pos($_) = $pos->{_value} if ref $pos;if ( _rule_not_newline() ) { Match::from_global_data( $GLOBAL::_M2 ); $MATCH = $GLOBAL::MATCH = pop @Match::Matches; } else { $MATCH = $GLOBAL::MATCH = Match->new(); } @Match::Matches = (); return $MATCH; } } ), )}
; 1 }
