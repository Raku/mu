{-# OPTIONS_GHC -fglasgow-exts #-}

module Pugs.Meta.Class () where
import Pugs.Val
import Pugs.Class

{-
instance Boxable Eval PureClass where
    classOf _ = _PureClass

_PureClass :: PureClass
_PureClass = mkBoxClass "Class"
    [ "HOW"         ... (const _PureClass :: PureClass -> PureClass)
    ]
-}
