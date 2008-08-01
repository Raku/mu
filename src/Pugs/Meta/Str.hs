{-# OPTIONS_GHC -fglasgow-exts -fno-warn-deprecations #-}

module Pugs.Meta.Str (_StrClass) where
import Data.Maybe
import Pugs.Val
import Pugs.Class
-- import qualified Data.ByteString.UTF8 as Str
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Char8 as Str -- XXX
import Pugs.Internals

instance Boxable PureStr where
    classOf _ = _StrClass

_StrClass :: PureClass
_StrClass = mkPureClass "Str"
    [ "reverse"     ... Str.reverse
    , "join"        ... Str.intercalate
    , "chop"        ... (\str -> if Str.null str then str else Str.init str)
    , "index"       ... (\str sub pos -> fromMaybe (-1) $ Str.findSubstring sub $ Str.drop pos str)
    , "chars"       ... Str.length   -- UTF8.length, which Does The Right Thing here
    , "bytes"       ... Char8.length -- WRONG! assumes in UTF-8 representation
    , "quotemeta"   ... Str.concatMap (cast . toQuoteMeta)
    , "split"       ... _split_str []
    ]

-- XXX: this is inefficient, since it scans the string about twice as
-- many times as really needed. a better implementation would meld the
-- findSubstring and drop steps inside a ByteString scan.
_split_str :: [PureStr] -> PureStr -> PureStr -> [PureStr]
_split_str accum str glue
    | Str.null str = accum
    | otherwise    = _split_str (accum ++ [next]) str' glue
    where
    (next, str') = case Str.findSubstring glue str of
        Just i  -> (Str.take i str, Str.drop (i + Str.length glue) str)
        Nothing -> (str, Str.empty)
