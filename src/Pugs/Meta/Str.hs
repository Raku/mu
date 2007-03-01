{-# OPTIONS_GHC -fglasgow-exts #-}

module Pugs.Meta.Str where
import Pugs.Val
import Pugs.Class
import qualified Data.ByteString.Char8 as Str

instance Boxable Eval PureStr where
    classOf _ = _StrClass

_StrClass :: PureClass
_StrClass = mkBoxClass "Str"
    [ "reverse"     ... (MkStr . Str.reverse . unStr)
    , "HOW"         ... (const _StrClass)
    , "WHICH"       ... id
    ]

