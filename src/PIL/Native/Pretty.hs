{-# OPTIONS_GHC -cpp -fglasgow-exts #-}

module PIL.Native.Pretty (Pretty(..), pretty, prettyM) where
import PIL.Native.Types
import PIL.Native.Coerce
import Text.PrettyPrint

{-| 

PIL.Native.Pretty

This is a pretty printer for the core runtime mini-language.

See Also:

  PIL.Native.Parser
  PIL.Native.Eval

-}

pretty :: (Pretty a) => a -> String
pretty = render . format

prettyM :: (MonadSTM m, Pretty a) => a -> m String
prettyM a = fmap render $ formatM a

defaultIndent :: Int
defaultIndent = 2

class (Show a) => Pretty a where
    format :: a -> Doc
    format = text . show
    formatM :: MonadSTM m => a -> m Doc
    formatM = return . format

instance Pretty (SeqOf NativeLangExpression) where
    format = format . elems

instance Pretty [NativeLangExpression] where
    format = sepBy semi

instance Pretty NativeLangExpression where
    format (ELit x) = format x
    format (EVar x) = format x
    format (ECall obj meth args) = hcat
        [ maybeParens obj                   -- obj
        , char '.', format meth             -- .method
        , parens (commaSep $ elems args)    -- (arg1, arg2)
        ]
        where
        maybeParens (ELit (NSub {})) = parens (format obj)
        maybeParens obj = format obj

instance Pretty NativeLangSym where
    format = text . toString

instance Pretty NativeSub where
    format (MkSub { s_params = params, s_exps = exps }) = hang
        (text "->" <+> commaSep (elems params)) defaultIndent
        (braces . format . elems $ exps)

instance Pretty Native where
    format (NError {})  = text "nil"
    format (NBit True)  = text "true"
    format (NBit False) = text "false"
    format (NInt x)     = int x
    format (NNum x)     = float x
    format (NStr x)     = format (toString x)
    format (NSeq x)     = brackets (commaSep $ elems x)
    format (NMap x)     = braces (commaSep $ assocs x)
    format (NSub x)     = format x
    format (NObj x)     = format x
    formatM (NObj x)    = formatM x
    formatM (NSeq x)    = formatM x
    formatM (NMap x)    = formatM x
    formatM x           = return (format x)

instance Pretty NativeObj where
    format o = text $ "<obj:#" ++ show (o_id o) ++ "|cls:#" ++ show (o_id (o_class o)) ++ ">"
    formatM o = do
        attrs <- liftSTM $ readTVar (o_attrs o)
        formatM attrs

instance Pretty NativeSeq where
    format x = brackets (nest defaultIndent (commaSep $ elems x))
    formatM x = do
        items <- commaSepM $ elems x
        return $ brackets (nest defaultIndent items)

instance Pretty NativeMap where
    format x = braces (nest defaultIndent (commaSep $ assocs x))
    formatM x = do
        pairs <- commaSepM $ assocs x
        return $ braces (nest defaultIndent pairs)

instance Pretty Doc where
    format = id

instance Pretty String where
    format = ptext . show

instance Pretty (NativeStr, Native) where
    format (x, y) = sep [format (toString x), text "=>", format y]

sepBy :: (Pretty a) => Doc -> [a] -> Doc
sepBy x = sep . punctuate x . map format

sepByM :: (MonadSTM m, Pretty a) => Doc -> [a] -> m Doc
sepByM x = fmap (sep . punctuate x) . mapM formatM

commaSep :: Pretty a => [a] -> Doc
commaSep = sepBy comma

commaSepM :: (MonadSTM m, Pretty a) => [a] -> m Doc
commaSepM = sepByM comma

