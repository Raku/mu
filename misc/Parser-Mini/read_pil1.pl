=pod 

  $ pugs -CPIL2 -e 'say "hello"'

 PIL_Environment
    { pilMain = (PStmts {pStmt = PNoop, pStmts = PStmts {pStmt = PPos 
 {pPos = (MkPos "-e" 1 1 1 12), pExp = App (Var "&say") Nothing [Pos 
 (MkPos "-e" 1 5 1 12) (Val (VStr "hello"))], pNode = PStmt {pExpr = PExp 
 {pLV = PApp {pCxt = TTailCall TCxtVoid, pFun = PExp {pLV = PVar 
 {pVarName = "&say"}}, pInv = Nothing, pArgs = [PLit {pLit = PVal {pVal = 
 VStr "hello"}}]}}}}, pStmts = PNil}})
    , pilGlob = ([PSub {pSubName = "&*END", pSubType = SubPrim, 
 pSubParams = [], pSubLValue = False, pSubIsMulti = False, pSubBody = 
 PNil}])
    }

=cut

    rule nam         { (<alpha>+) };
    rule str         { \" .*? \" };
    rule spc         { <space> | \n };
    rule block       { \{ <spc>* <nam> <spc>* \= .* \} };
    rule parenthesis { \( ( <anything> | <spc> ) * \) };
    rule square      { \[ ( <anything> | <spc> ) * \] };
    rule anything    { <block> | <parenthesis> | <square> | <nam> | 
                       <str> | <spc> 
                     };
    rule pil2        { <nam> <spc> \{ <anything>* \} };

say "match digit - error!" if "123" ~~ /<nam>/;
say "match name"  if "abc" ~~ /<nam>/;
say "match block" if '{ xxx = a123 }' ~~ /<block>/;
say "match parenthesis"  if '(a b c)' ~~ /<parenthesis>/;
say "match square"  if '[a b c]' ~~ /<square>/;
say "match str"     if '"abc"'  ~~ /<str>/;

my $pil2 = q/
 PIL_Environment
    { pilMain = (PStmts {pStmt = PNoop, pStmts = PStmts {pStmt = PPos
 {pPos = (MkPos "-e" 1 1 1 12), pExp = App (Var "&say") Nothing [Pos
 (MkPos "-e" 1 5 1 12) (Val (VStr "hello"))], pNode = PStmt {pExpr = PExp
 {pLV = PApp {pCxt = TTailCall TCxtVoid, pFun = PExp {pLV = PVar
 {pVarName = "&say"}}, pInv = Nothing, pArgs = [PLit {pLit = PVal {pVal =
 VStr "hello"}}]}}}}, pStmts = PNil}})
    , pilGlob = ([PSub {pSubName = "&*END", pSubType = SubPrim,
 pSubParams = [], pSubLValue = False, pSubIsMulti = False, pSubBody =
 PNil}])
    }
/;

say $pil2;

my $b = $pil2 ~~ m/<pil2>/;
say $b.perl;

my $c = $pil2 ~~ / <nam> <spc> \{ /;
say $c.perl;
