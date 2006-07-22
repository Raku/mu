
use lib '../Pugs-Grammar-MiniPerl6/lib';
use Test::More tests => 6;

use_ok( 'Pugs::Grammar::Rule' );
use_ok( 'Pugs::Emitter::Rule::Parsec' );

# XXX replace by really feed result to GHC and parse test data
sub is_rule_match {
    my ($input, $output, @arg) = @_;
    my $tree = Pugs::Grammar::Rule->rule($input);
    my $got = Pugs::Emitter::Rule::Parsec::emit(0, $tree->{capture}, 0);
    @_ = ($got, $output, @arg);
    goto \&is;
}

is_rule_match(
q#
        $<sym> := [ <'...'> | <'???'> | <'!!!'> ]
        {
	  return App(
            Var( doYada( $<sym> ) ),
            Nothing,
            [ Val( VStr( $<sym> ~ " - not yet implemented") ) ]
          )
        }
#,
q#do
  sym <- do
      string "..."
      <|>
      string "???"
      <|>
      string "!!!"
  return $ (App (Var (doYada sym)) Nothing [(Val (VStr (sym ++ " - not yet implemented")))])
#,
'yadaLiteral');

is_rule_match(
q#
	\\^ | \\* | \\? | \\. | \\! | \\+ | ; | <''>
#,
q#do
  string "^"
  <|>
  string "*"
  <|>
  string "?"
  <|>
  string "."
  <|>
  string "!"
  <|>
  string "+"
  <|>
  string ";"
  <|>
  string ""
#,
'ruleTwigil');

is_rule_match(
q#
	$<sigil>  := [ \$ | \@ | \% ]
	$<digits> := [ <digit>+ ]
	{ return $<sigil> ~ $<digits> }
#,
q#do
  sigil <- do
      string "$"
      <|>
      string "@"
      <|>
      string "%"
  digits <- ((many1 $ (digit >>= \c -> return [c])) >>= \arr -> return $ foldr (++) "" arr)
  return $ (sigil ++ digits)
#,
'ruleMatchPos');

is_rule_match(
q#
	$<sigil> := [ \$ | \@ | \% ]
	\<
	$<name>  := [ [ \\ . | <-[\>]> ]* ]
	\>
	{ return $<sigil> ~ "<" ~ $<name> ~ ">" }
#,
q#do
  sigil <- do
      string "$"
      <|>
      string "@"
      <|>
      string "%"
  string "<"
  name <- ((many $ do
        do
          string " "
          (anyChar >>= \c -> return [c])
        <|>
        (noneOf ">" >>= \c -> return [c])) >>= \arr -> return $ foldr (++) "" arr)
  string ">"
  return $ (sigil ++ ("<" ++ (name ++ ">")))
#,
'ruleMatchNamed');

