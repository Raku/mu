{-# OPTIONS_GHC -fglasgow-exts #-}

module Pugs.Meta.Str where
import Pugs.Val
import Pugs.Class
import qualified Data.ByteString.Char8 as Str

instance Boxable PureStr where
    classOf _ = _StrClass

_StrClass :: PureClass
_StrClass = mkPureClass "Str"
    [ "reverse"     ... Str.reverse
    , "join"        ... (\self args -> Str.join self args)
    ]

