{-# OPTIONS_GHC -fglasgow-exts #-}

module PIL.Pad (Pad, Sym(..), Sigil(..), Twigil, mkSym, unSigil) where
import PIL.Container
import PIL.Internals

-- Pad maps symbols to containers
newtype Pad = MkPad (Map Sym Container)

mkSym :: (Monad m) => String -> m Sym
mkSym (':':n@(':':_)) = mkSym n
mkSym (s:name)    = do
    sig <- mkSigil s
    return $ MkSym
        { symSigil      = sig
        , symTwigil     = TwigilNone    -- XXX Stub
        , symPackage    = []            -- XXX Stub
        , symName       = MkName name
        }
mkSym [] = fail "Empty symbol"

mkSigil :: (Monad m) => Char -> m Sigil
mkSigil '$' = return SigilScalar
mkSigil '@' = return SigilArray
mkSigil '%' = return SigilHash
mkSigil '&' = return SigilCode
mkSigil ':' = return SigilPackage
mkSigil s   = fail $ "Unknown sigil: " ++ [s]

unSigil :: Sigil -> Char
unSigil SigilScalar = '$'
unSigil SigilArray  = '@'
unSigil SigilHash   = '%'
unSigil SigilCode   = '&'
unSigil SigilPackage= ':'

--  $?CALLER::CALLER::SUB
data Sym = MkSym
    { symSigil      :: Sigil       --  $
    , symTwigil     :: Twigil      --  ?
    , symPackage    :: [Name]      --  [CALLER, CALLER]
    , symName       :: Name        --  SUB
    }
    deriving (Eq, Ord, Show, Typeable)

data Sigil
    = SigilScalar   --  $
    | SigilArray    --  @
    | SigilHash     --  %
    | SigilCode     --  &
    | SigilPackage  --  ::
    deriving (Eq, Ord, Show, Enum, Typeable)

{-|
A twigil (secondary sigil) represents a particular special type of variable.

Examples of each include:

* Lexically-scoped magical: @$?SELF@

* Globally-available: @%*ENV@

* File-scoped magical: @%=POD@

* Implicit parameter in bare-block: @{ $^a + $^b }@

* Public member: @has $.skin@

* Private member: @has $:guts@
-}
data Twigil
    = TwigilNone     -- ^ No twigil (most variables)
    | TwigilCompiled -- ^ @?@
    | TwigilGlobal   -- ^ @*@
    | TwigilFile     -- ^ @=@
    | TwigilImplicit -- ^ @^@
    | TwigilPublic   -- ^ @.@
    | TwigilPrivate  -- ^ @:@
    deriving (Eq, Ord, Show, Enum, Typeable)

