{-# OPTIONS_GHC -fglasgow-exts -fparr #-}

module Pugs.Meta.Str where
import Pugs.Val
import Pugs.Class
import qualified Data.ByteString.Char8 as Str

instance Boxable PureStr where
    classOf _ = _StrClass

_StrClass :: PureClass
_StrClass = mkPureClass "Str"
    [ "reverse"     ... (MkStr . Str.reverse . unStr)
    , "join"        ... (\self values -> MkStr (Str.join (unStr self) (map unStr values)))
    ]

