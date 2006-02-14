#!/usr/bin/perl 

use Test;
BEGIN { plan tests => 5 };
use Rule::Parser::OpTable;

sub say {
print @_ . "\n";
}

{
  $optable = new Rule::Parser::OpTable;
  $optable->addToken("term:", "", "nows", "");
  $optable->addToken("term:::", "term:", "nows", "PGE::Exp::Cut");
  $optable->addToken("term::::", "term:", "nows", "PGE::Exp::Cut");
  $optable->addToken("term:\\b", "term:", "nows", "PGE::Exp::Anchor");
  $optable->addToken("term:\\B", "term:", "nows", "PGE::Exp::Anchor");
  $optable->addToken("term:^", "term:", "nows", "PGE::Exp::Anchor");
  $optable->addToken("term:^^", "term:", "nows", "PGE::Exp::Anchor");
  $optable->addToken("term:$$", "term:", "nows", "PGE::Exp::Anchor");
=pod

    #$P0 = find_global "PGE::P6Rule", "parse_dollar"
    #optable.addtok("term:$", "term:", "nows", $P0)

    optable.addtok("term:.", "term:", "nows", "PGE::Exp::CCShortcut")
    optable.addtok("term:\\d", "term:", "nows", "PGE::Exp::CCShortcut")
    optable.addtok("term:\\D", "term:", "nows", "PGE::Exp::CCShortcut")
    optable.addtok("term:\\s", "term:", "nows", "PGE::Exp::CCShortcut")
    optable.addtok("term:\\S", "term:", "nows", "PGE::Exp::CCShortcut")
    optable.addtok("term:\\w", "term:", "nows", "PGE::Exp::CCShortcut")
    optable.addtok("term:\\W", "term:", "nows", "PGE::Exp::CCShortcut")
    optable.addtok("term:\\n", "term:", "nows", "PGE::Exp::CCShortcut")
    optable.addtok("circumfix:[ ]", "term:", "nows", "PGE::Exp::Group")
    optable.addtok("circumfix:( )", "term:", "nows", "PGE::Exp::Group")

    optable.addtok("<commit>", "term:", "nows", "PGE::Exp::Commit")

    $P0 = find_global "PGE::P6Rule", "parse_subrule"
    optable.addtok("term:<", "term:", "nows", $P0)
    optable.addtok("term:<?", "term:", "nows", $P0)

    $P0 = find_global "PGE::P6Rule", "parse_enumclass"
    optable.addtok("term:<[", "term:", "nows", $P0)
    optable.addtok("term:<-[", "term:", "nows", $P0)
    optable.addtok("term:<+[", "term:", "nows", $P0)

    $P0 = find_global "PGE::P6Rule", "parse_closure"
    optable.addtok("term:{{", "term:", "nows", $P0)

    $P0 = find_global "PGE::P6Rule", "parse_quant"
    optable.addtok("postfix:*", "<term:", "left", $P0)
    optable.addtok("postfix:+", "postfix:*", "left", $P0)
    optable.addtok("postfix:?", "postfix:*", "left", $P0)
    optable.addtok("postfix::", "postfix:*", "left", "PGE::Exp::Cut")
    $P0 = find_global "PGE::Rule", "fail"
    optable.addtok("postfix:::", "postfix:*", "left", $P0)

    optable.addtok("infix:", "<postfix:*", "right,nows", "PGE::Exp::Concat")
    optable.addtok("infix:&", "<infix:", "left,nows", "PGE::Exp::Conj")
    optable.addtok("infix:|", "<infix:&", "left,nows", "PGE::Exp::Alt")

    optable.addtok("infix::=", ">postfix:*", "right", "PGE::Exp::Alias")

    $P0 = find_global "PGE::P6Rule", "parse_modifier"
    optable.addtok("prefix::", "<infix:|", "nows", $P0)
=cut
}

