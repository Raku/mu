{-# OPTIONS_GHC -fglasgow-exts -fallow-overlapping-instances #-}
module Pugs.Parser.Util where

import Pugs.Internals
import Pugs.AST
import Pugs.Types
import Pugs.Lexer
import Pugs.Rule

import Pugs.Parser.Types
import qualified Data.Map as Map
import qualified Data.Set as Set

fixities :: [String]
fixities = ["prefix_circumfix_meta_operator:","infix_circumfix_meta_operator:","prefix_postfix_meta_operator:","postfix_prefix_meta_operator:","infix_postfix_meta_operator:","statement_modifier:","statement_control:","scope_declarator:","trait_auxiliary:","trait_verb:","regex_mod_external:","regex_mod_internal:","regex_assertion:","regex_backslash:","regex_metachar:","postcircumfix:","circumfix:","postfix:","infix:","prefix:","quote:","term:"]

-- around a block body we save the package and the current lexical pad
-- at the start, so that they can be restored after parsing the body
localEnv :: RuleParser Exp -> RuleParser Exp
localEnv m = do
    state   <- get
    let env = s_env state
    put state
        { s_blockPads = Map.empty
        , s_outerVars = Set.empty
        , s_env = env { envOuter = Just env }
        }
    rv      <- m
    state'  <- get
    put state
        { s_env = (s_env state')
            { envPackage = envPackage env
            , envLexical = envLexical env
            , envOuter   = envOuter env
            }
        }
    -- Hoist all pad-declared entries into this block
    -- XXX - Handle "state" and "constant" here.
    return $ Map.foldWithKey Pad rv (s_blockPads state')

ruleParamList :: ParensOption -> RuleParser a -> RuleParser (Maybe [[a]])
ruleParamList wantParens parse = rule "parameter list" $ do
    (formal, hasParens) <- f $
        (((try parse) `sepEndBy` lexeme (oneOf ",;")) `sepEndBy` invColon)
    case formal of
        [[]]   -> return $ if hasParens then Just [[], []] else Nothing
        [args] -> return $ Just [[], args]
        [_,_]  -> return $ Just formal
        _      -> fail "Only one invocant list allowed"
    where
    f = case wantParens of
        ParensOptional  -> maybeParensBool
        ParensMandatory -> \x -> do rv <- parens x; return (rv, True)
    invColon = do
        ch <- oneOf ":;"
        -- Compare:
        --   sub foo (: $a)   # vs.
        --   sub foo (:$a)
        lookAhead $ (many1 space <|> string ")")
        whiteSpace
        return ch
        
maybeParensBool :: RuleParser a -> RuleParser (a, Bool)
maybeParensBool p = choice
    [ do rv <- parens p; return (rv, True)
    , do rv <- p; return (rv, False)
    ]


isOperatorName :: String -> Bool
isOperatorName ('&':name) = any hasOperatorPrefix [name, tail name]
    where
    hasOperatorPrefix :: String -> Bool
    hasOperatorPrefix name = any (`isPrefixOf` name) fixities
isOperatorName _ = False


{-| Wraps a call to @&Pugs::Internals::check_for_io_leak@ around the input
    expression. @&Pugs::Internals::check_for_io_leak@ should @die()@ if the
    expression returned an IO handle. -}
-- Please remember to edit Prelude.pm, too, if you rename the name of the
-- checker function.
checkForIOLeak :: Exp -> Exp
checkForIOLeak exp =
    App (_Var "&Pugs::Internals::check_for_io_leak") Nothing
        [ Val $ VCode mkSub { subBody = exp } ]
    
defaultParamFor :: SubType -> [Param]
defaultParamFor SubBlock    = [defaultScalarParam]
defaultParamFor SubPointy   = []
defaultParamFor _           = [defaultArrayParam]

_dollarUnderscore :: Var
_dollarUnderscore = cast "$_"

doExtract :: SubType -> Maybe [Param] -> Exp -> (Exp, [Var], [Param])
doExtract SubBlock formal body = (fun, names', params)
    where
    (fun, names) = extractPlaceholderVars body []
    names' | isJust formal
           = filter (/= _dollarUnderscore) names
           | otherwise
           = names
    params = map nameToParam (sort names') ++ (maybe [] id formal)
doExtract SubPointy formal body = (body, [], maybe [] id formal)
doExtract SubMethod formal body = (body, [], maybe [] id formal)
doExtract _ formal body = (body, names', params)
    where
    (_, names) = extractPlaceholderVars body []
    names' | isJust formal
           = filter (/= _dollarUnderscore) names
           | otherwise
           = filter (== _dollarUnderscore) names
    params = map nameToParam (sort names') ++ (maybe [] id formal)

nameToParam :: Var -> Param
nameToParam name = MkParam
    { isInvocant    = False
    , isOptional    = False
    , isNamed       = False
    , isLValue      = True
    , isWritable    = (name == _dollarUnderscore)
    , isLazy        = False
    , paramName     = name
    , paramContext  = CxtItem $ typeOfSigilVar name
    , paramDefault  = Noop
    }

_percentUnderscore :: Var
_percentUnderscore = cast "%_"

paramsFor :: SubType -> Maybe [Param] -> [Param] -> [Param]
paramsFor SubMethod formal params 
    | isNothing (find ((_percentUnderscore ==) . paramName) params)
    = paramsFor SubRoutine formal params ++ [defaultHashParam]
paramsFor styp Nothing []       = defaultParamFor styp
paramsFor _ _ params            = params

processFormals :: Monad m => [[Exp]] -> m (Maybe Exp, [Exp])
processFormals formal = case formal of
    []      -> return (Nothing, [])
    [args]  -> return (Nothing, unwind args)
    [invs,args] | [inv] <- unwind invs -> return (Just inv, unwind args)
    _                   -> fail "Only one invocant allowed"
    where
    unwind :: [Exp] -> [Exp]
    unwind [] = []
    unwind ((Syn "," list):xs) = unwind list ++ unwind xs
    unwind x  = x

-- | A Param representing the default (unnamed) invocant of a method on the given type.
selfParam :: Type -> Param
selfParam typ = MkParam
    { isInvocant    = True
    , isOptional    = False
    , isNamed       = False
    , isLValue      = True
    , isWritable    = True
    , isLazy        = False
    , paramName     = cast "&self"
    , paramContext  = CxtItem typ
    , paramDefault  = Noop
    }

extractHash :: Exp -> Maybe Exp
extractHash exp = extractHash' (possiblyUnwrap exp)
    where
    possiblyUnwrap (Syn "block" [exp]) = exp
    possiblyUnwrap (App (Val (VCode (MkCode { subType = SubBlock, subBody = fun }))) Nothing []) = fun
    possiblyUnwrap x = x
    
    isHashOrPair (Ann _ exp) = isHashOrPair exp
    isHashOrPair (App (Var var) _ _) =
        v_sigil var == SHash || (var == cast "&pair") || (var == cast "&infix:=>") 
    isHashOrPair (Syn "%{}" _) = True
    isHashOrPair _ = False
    
    extractHash' (Ann _ exp) = extractHash' exp
    extractHash' exp                      | isHashOrPair exp    = Just exp
    extractHash' exp@(Syn "," (subexp:_)) | isHashOrPair subexp = Just exp
    extractHash' exp@Noop = Just exp
    extractHash' _ = Nothing

tryLookAhead :: RuleParser a -> RuleParser b -> RuleParser a
tryLookAhead rule after = try $ do
    rv <- rule
    lookAhead after
    return rv

makeVar :: String -> Exp
makeVar (s:"<>") =
    makeVarWithSigil s $ _Var "$/"
makeVar (s:rest) | all (`elem` "1234567890") rest =
    makeVarWithSigil s $ Syn "[]" [_Var "$/", Val $ VInt (read rest)]
makeVar (s:'<':name) =
    makeVarWithSigil s $ Syn "{}" [_Var "$/", doSplitStr (init name)]
makeVar var = _Var var

makeVarWithSigil :: Char -> Exp -> Exp
makeVarWithSigil '$' x = x
makeVarWithSigil s   x = Syn (s:"{}") [x]

-- | splits the string into expressions on whitespace.
-- Implements the <> operator at parse-time.
doSplitStr :: String -> Exp
doSplitStr str = case perl6Words str of
    []  -> Syn "," []
    [x] -> Val (VStr x)
    xs  -> Syn "," $ map (Val . VStr) xs
    where
    perl6Words :: String -> [String]
    perl6Words s
      | findSpace == [] = []
      | otherwise       = w : words s''
      where
      (w, s'')  = break isBreakingSpace findSpace
      findSpace = dropWhile isBreakingSpace s
      
    isBreakingSpace('\x09') = True
    isBreakingSpace('\x0a') = True
    isBreakingSpace('\x0d') = True
    isBreakingSpace('\x20') = True
    isBreakingSpace(_)      = False

tryFollowedBy :: RuleParser a -> RuleParser b -> RuleParser a
tryFollowedBy rule after = try $ do
    rv <- rule
    after
    return rv

