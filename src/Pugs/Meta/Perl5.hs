{-# OPTIONS_GHC -fglasgow-exts #-}

module Pugs.Meta.Perl5 where
import Pugs.Val
import Pugs.Class
import Pugs.Embed.Perl5
import Pugs.Internals
import Data.Typeable (Typeable)
import qualified Data.Map as Map
import {-# SOURCE #-} Pugs.AST.Internals (envContext, anyToVal, anyFromVal)
import Pugs.Types

data Perl5Responder = Perl5Responder deriving Typeable

instance ResponderInterface Eval Perl5Responder where
    dispatch _          = dispatchPerl5
    fromMethodList _    = return Perl5Responder
    toNameList _        = []

instance Boxable Eval PerlSV where
    mkObj sv = MkInvocant sv (MkResponder (return Perl5Responder))
    fromObj (MkInvocant x _)
        | Just x' <- fromTypeable x = return x'
        | Just x' <- fromTypeable x = liftIO $ vstrToSV x'
        | Just x' <- fromTypeable x = liftIO . bufToSV  $ (cast :: PureStr -> ByteString) x'
        | Just x' <- fromTypeable x = liftIO . vintToSV $ (cast :: PureInt -> Integer)    x'
        | Just x' <- fromTypeable x = liftIO . vnumToSV $ (cast :: PureNum -> Double)     x'
        | Just x' <- fromTypeable x = anyFromVal x'
        | otherwise                 = fail $ "Cannot coerce to SV: " ++ show (typeOf x)


dispatchPerl5 :: Val -> Call -> Eval Val
dispatchPerl5 inv call = do
    let feed = concatFeeds (c_feeds (miArguments call))
    invSV   <- fromObj inv
    subSV   <- liftIO . bufToSV . cast $ miName call
    posSVs  <- mapM fromObj (fromP $ f_positionals feed)
    namSVs  <- fmap concat . forM (Map.toList (f_nameds feed)) $ \(key, vals) -> do
        keySV   <- liftIO (bufToSV $ cast key)
        fmap concat . forM (fromP vals) $ \v -> do
            valSV   <- fromObj v
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
