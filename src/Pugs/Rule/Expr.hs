-----------------------------------------------------------------------------
-- |
-- Module      :  Rule.Expr
-- Copyright   :  (c) Daan Leijen 1999-2001
-- License     :  BSD-style (see the file libraries/parsec/LICENSE)
--
-- Maintainer  :  daan@cs.uu.nl
-- Stability   :  provisional
-- Portability :  portable
--
-- A helper module to parse \"expressions\".
-- Builds a parser given a table of operators and associativities.
--
-----------------------------------------------------------------------------

module Pugs.Rule.Expr
                 ( Assoc(..), Operator(..), OperatorTable
                 , buildExpressionParser
                 ) where

import Pugs.Rule.Prim
import Pugs.Rule.Combinator


-----------------------------------------------------------
-- Assoc and OperatorTable
-----------------------------------------------------------
data Assoc                = AssocNone
                          | AssocLeft
                          | AssocRight
                          | AssocList
                          | AssocChain

data Operator t st a      = Infix (GenParser t st (a -> a -> a)) Assoc
                          | InfixList (GenParser t st ([a] -> a)) Assoc
                          | Prefix (GenParser t st (a -> a))
                          | Postfix (GenParser t st (a -> a))
                          | OptionalPrefix (GenParser t st (a -> a))

type OperatorTable t st a = [[Operator t st a]]



-----------------------------------------------------------
-- Convert an OperatorTable and basic term parser into
-- a full fledged expression parser
-----------------------------------------------------------
buildExpressionParser :: OperatorTable tok st a -> GenParser tok st a -> a -> GenParser tok st a
buildExpressionParser operators simpleExpr emptyExpr
    = foldl (makeParser) simpleExpr operators
    where
      makeParser term ops
        = let (rassoc,lassoc,nassoc
               ,prefix,postfix,optPrefix,listAssoc) = foldr splitOp ([],[],[],[],[],[],[]) ops

              rassocOp   = choice rassoc
              lassocOp   = choice lassoc
              nassocOp   = choice nassoc
              prefixOp   = choice prefix <?> ""
              postfixOp  = choice postfix <?> ""
              optPrefixOp= choice optPrefix <?> ""
              listAssocOp= choice listAssoc

              ambigious assoc op= try $
                                  do{ op; fail ("ambiguous use of a " ++ assoc
                                                 ++ " associative operator")
                                    }

              ambigiousRight    = ambigious "right" rassocOp
              ambigiousLeft     = ambigious "left" lassocOp
              ambigiousNon      = ambigious "non" nassocOp

              foldOp = foldr (.) id

              termP = do
                pres    <- many $ (return . Left =<< prefixOp) <|> (return . Right =<< optPrefixOp)
                -- ok. here we do this trick thing
                x <- if null pres
                    then term
                    else case last pres of
                        Left _ -> term
                        _ -> option emptyExpr term
                posts   <- many postfixOp
                return $ foldOp posts $ foldOp (map (either id id) pres) x

              rassocP x  = do{ f <- rassocOp
                             ; y  <- do{ z <- termP; rassocP1 z }
                             ; return (f x y)
                             }
                           <|> ambigiousLeft
                           <|> ambigiousNon
                           -- <|> return x

              rassocP1 x = rassocP x  <|> return x

              lassocP x  = do{ f <- lassocOp
                             ; y <- termP
                             ; lassocP1 (f x y)
                             }
                           <|> ambigiousRight
                           <|> ambigiousNon
                           -- <|> return x

              lassocP1 x = lassocP x <|> return x

              nassocP x  = do{ f <- nassocOp
                             ; y <- termP
                             ;    ambigiousRight
                              <|> ambigiousLeft
                              <|> ambigiousNon
                              <|> return (f x y)
                             }
                           -- <|> return x

              listAssocP x  = do
                f   <- listAssocOp
                xs  <- option [] $ listAssocP1 =<< termP
                return $ f (x:xs)

              listAssocP0 x  = do
                listAssocOp
                xs  <- option [] $ listAssocP1 =<< termP
                return (x:xs)
              listAssocP1 x = listAssocP0 x <|> return [x]

           in  do{ x <- termP
                 ; rassocP x <|> lassocP  x <|> nassocP x <|> listAssocP x <|> return x
                   <?> "operator"
                 }


      splitOp (Infix op assoc) (rassoc,lassoc,nassoc,prefix,postfix,optPrefix,listAssoc)
        = case assoc of
            AssocNone  -> (rassoc,lassoc,op:nassoc,prefix,postfix,optPrefix,listAssoc)
            AssocLeft  -> (rassoc,op:lassoc,nassoc,prefix,postfix,optPrefix,listAssoc)
            AssocRight -> (op:rassoc,lassoc,nassoc,prefix,postfix,optPrefix,listAssoc)
            _          -> error "splitOp: unimplemented assoc type."

      splitOp (InfixList op assoc) (rassoc,lassoc,nassoc,prefix,postfix,optPrefix,listAssoc)
        = case assoc of
            AssocList  -> (rassoc,lassoc,nassoc,prefix,postfix,optPrefix,op:listAssoc)
            -- FIXME: add AssocChain
            _          -> error "splitOp: unimplemented assoc type."

      splitOp (Prefix op) (rassoc,lassoc,nassoc,prefix,postfix,optPrefix,listAssoc)
        = (rassoc,lassoc,nassoc,op:prefix,postfix,optPrefix,listAssoc)

      splitOp (Postfix op) (rassoc,lassoc,nassoc,prefix,postfix,optPrefix,listAssoc)
        = (rassoc,lassoc,nassoc,prefix,op:postfix,optPrefix,listAssoc)

      splitOp (OptionalPrefix op) (rassoc,lassoc,nassoc,prefix,postfix,optPrefix,listAssoc)
        = (rassoc,lassoc,nassoc,prefix,postfix,op:optPrefix,listAssoc)
