{-# OPTIONS_GHC -fglasgow-exts #-}

module Pugs.Meta.Str where
import Data.Maybe
import Pugs.Val
import Pugs.Class
import Prelude (($))
import qualified Prelude as P
import UTF8

instance Boxable PureStr where
    classOf _ = _StrClass

_StrClass :: PureClass
_StrClass = mkPureClass "Str"
    [ "reverse"     ... reverse
    , "join"        ... join
    , "chop"        ... (\str -> if null str then str else init str)
    , "index"       ... (\str sub pos -> fromMaybe (-1) $ findSubstring sub $ drop pos str)
    ]

