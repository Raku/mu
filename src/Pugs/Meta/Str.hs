{-# OPTIONS_GHC -fglasgow-exts #-}

module Pugs.Meta.Str where
import Pugs.Val
import Pugs.Class
import qualified UTF8 as Str
import Data.Maybe

instance Boxable PureStr where
    classOf _ = _StrClass

_StrClass :: PureClass
_StrClass = mkPureClass "Str"
    [ "reverse"     ... Str.reverse
    , "join"        ... Str.join
    , "chop"        ... (\str -> if Str.null str then str else Str.init str)
    , "index"       ... (\str sub pos -> fromMaybe (-1) $ Str.findSubstring sub $ Str.drop pos str)
    ]

