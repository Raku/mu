{-# OPTIONS_GHC -fglasgow-exts #-}

module PIL where
import PIL.Val
import PIL.Container
import PIL.Internals

-- Pad maps symbols to containers
newtype Pad = MkPad (Map Sym Container)

--  $?CALLER::CALLER::SUB
data Sym = MkSym
    { symSigil      :: Sigil       --  $
    , symTwigil     :: Twigil      -- ?
    , symPackage    :: [Name]      -- [CALLER, CALLER]
    , symName       :: Name        -- SUB
    }
    deriving (Eq, Ord, Show, Typeable)

data Sigil
    = SigilScalar   --  $
    | SigilArray    -- @
    | SigilHash     -- %
    | SigilCode     -- &
    | SigilPackage  -- ::
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

