
use lib '../Pugs-Grammar-MiniPerl6/lib';
use Test::More tests => 9;

use Pugs::Grammar::Rule;
use_ok( 'Pugs::Emitter::Rule::Parsec' );

# XXX replace by really feed result to GHC and parse test data
sub is_rule_match {
    my ($input, $output, $emit_arg, @arg) = @_;
    if(ref $emit_arg ne 'HASH'){
	unshift @arg, $emit_arg;
	$emit_arg = { };
    }

    my $tree = Pugs::Grammar::Rule->rule($input);
    my $got = Pugs::Emitter::Rule::Parsec::emit({ }, $tree->{capture},
	    $emit_arg);
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
	[ \\^ | \\* | \\? | \\. | \\! | \\+ | ; ]?
#,
q#option "" $ do
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
	$<name>  := [ [ \\\\ . | <-[\>]> ]* ]
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
          string "\\\\"
          (anyChar >>= \c -> return [c])
        <|>
        (noneOf ">" >>= \c -> return [c])) >>= \arr -> return $ foldr (++) "" arr)
  string ">"
  return $ (sigil ++ ("<" ++ (name ++ ">")))
#,
'ruleMatchNamed');

is_rule_match(
q#
	[ , | ; ] <?ws>?
	{ return; }
#,
q#do
  do
    string ","
    <|>
    string ";"
  option "" $ (whiteSpace >> return "")
  return ()
#,
'ruleCommaOrSemicolon');

is_rule_match(
q#
	[ \\. <!before \\.> | <?longDot> ]
	[ \\* | \\+ | \\? ]?
	{ return }
#,
q#do
  do
    do
      string "."
      notFollowedBy $ (string ".") >> return ' '
    <|>
    ruleLongDot
  option "" $ do
      string "*"
      <|>
      string "+"
      <|>
      string "?"
  return ()
#,
'ruleDot (not "try"ed)');

is_rule_match(
q#\\\\<!before \\(> \\.{ return }#,
q#do
  string "\\\\"
  notFollowedBy $ (string "(") >> return ' '
  (whiteSpace >> return "")
  string "."
  return ()
#,
{ sigspace => 1 },
'ruleLongDot (not "try"ed)');

is_rule_match(
q#
	(<'for'>|<'while'>|<'until'>) : <?ws>?
	$<exp1> := <perl6_expression("no_blocks",0)> <?ws>?
	$<exp2> := <block>
        { return mkHash([
                    "statement", $0,
                    "exp1", $<exp1>,
                    "exp2", $<exp2>
		])
        }
#,
q#do
  capture_0 <- do
      string "for"
      <|>
      string "while"
      <|>
      string "until"
  option "" $ (whiteSpace >> return "")
  exp1 <- rulePerl6_expression ("no_blocks") (0)
  option "" $ (whiteSpace >> return "")
  exp2 <- ruleBlock
  return $ (mkHash ["statement", capture_0, "exp1", exp1, "exp2", exp2])
#,
'ruleLoop');

