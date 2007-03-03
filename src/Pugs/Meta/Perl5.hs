{-# OPTIONS_GHC -fglasgow-exts #-}

module Pugs.Meta.Perl5 where
import Pugs.Val
import Pugs.Class
import Pugs.Embed.Perl5
import Data.Typeable

data Perl5Responder = Perl5Responder deriving Typeable

instance ResponderInterface Eval Perl5Responder where
    dispatch _          = dispatchPerl5
    fromMethodList _    = return Perl5Responder
    toNameList _        = []

instance Boxable Eval PerlSV where
    mkObj sv = MkInvocant sv (MkResponder (return Perl5Responder))

dispatchPerl5 :: Val -> Call -> Eval Val
dispatchPerl5 inv call = do
    fail $ "Dispatch failed - " ++ show (miName call)
