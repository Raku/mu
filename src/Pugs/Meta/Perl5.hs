{-# OPTIONS_GHC -fglasgow-exts #-}

module Pugs.Meta.Perl5 (Perl5Responder) where
import Pugs.Val
import Pugs.Class
import Pugs.Embed.Perl5
import Pugs.Internals
import Data.Typeable (Typeable)
import qualified Data.Map as Map
import qualified StringTable.AtomMap as AtomMap
import Pugs.AST.Internals (envContext, anyToVal, anyFromVal)
import Pugs.Types

data Perl5Responder = Perl5Responder deriving Typeable

instance ResponderInterface Eval Perl5Responder where
    dispatch _          = dispatchPerl5
    fromMethodList _    = return Perl5Responder

instance Boxable PerlSV where
    mkVal sv = MkInvocant sv (MkResponder (return Perl5Responder))
    coerceVal (MkInvocant x _)
        | Just x' <- fromTypeable x = return x'
        | Just x' <- fromTypeable x = liftIO $ vstrToSV x'
        | Just x' <- fromTypeable x = liftIO . bufToSV  $ (cast :: PureStr -> ByteString) x'
        | Just x' <- fromTypeable x = liftIO . vintToSV $ (cast :: PureInt -> Integer)    x'
        | Just x' <- fromTypeable x = liftIO . vnumToSV $ (cast :: PureNum -> Double)     x'
        | Just x' <- fromTypeable x = anyFromVal x'
        | otherwise                 = fail $ "Cannot coerce to SV: " ++ show (typeOf x)

__ITEM__, __LIST__ :: MethodName
__ITEM__    = _cast "ITEM"
__LIST__    = _cast "LIST"

dispatchPerl5 :: Val -> Call -> Eval Val
dispatchPerl5 inv call
    | meth == nullID    = return inv -- XXX - real HOW support --
    | meth == __ITEM__  = return inv -- XXX - real rvalue suport --
    | meth == __LIST__  = return inv -- XXX - real lvalue suport --
    | otherwise = do
        invSV   <- coerceVal inv
        subSV   <- liftIO . bufToSV . cast $ meth
        posSVs  <- mapM coerceVal (fromP $ f_positionals feed)
        namSVs  <- fmap concat . forM (AtomMap.toList (f_nameds feed)) $ \(key, vals) -> do
            keySV   <- liftIO (bufToSV $ cast key)
            fmap concat . forM (fromP vals) $ \v -> do
                valSV   <- coerceVal v
                return [keySV, valSV]
        env     <- ask
        rv      <- liftIO $ do
            envSV   <- mkEnv env
            invokePerl5 subSV invSV (posSVs ++ namSVs) envSV (enumCxt $ envContext env)
        case rv of
            Perl5ReturnValues [x]   -> return $ mkVal x 
            Perl5ReturnValues xs    -> return $ mkVal xs
            Perl5ErrorString str    -> fail str
            Perl5ErrorObject err    -> throwError (anyToVal err)
    where
    meth = mi_name call
    feed = concatFeeds (c_feeds (mi_arguments call))
