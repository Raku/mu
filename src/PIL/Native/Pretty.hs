{-# OPTIONS_GHC -cpp -fglasgow-exts #-}

module PIL.Native.Pretty (Pretty(..), pretty) where
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

pretty :: Pretty a => a -> String
pretty a = render $ format a

class (Show a) => Pretty a where
    format :: a -> Doc
    format x = text $ show x

instance Pretty [NativeLangExpression] where
    format = sepBy semi

#ifndef HADDOCK
instance Pretty NativeLangExpression where
    format (NL_Lit x) = format x
    format (NL_Var x) = format x
    format (NL_Call obj meth args) = hcat
        [ maybeParens obj                   -- $obj
        , char '.', format meth             -- .method
        , parens (commaSep $ elems args)    -- ($arg1, $arg2)
        ]
        where
        maybeParens (NL_Lit (NBlock {})) = parens (format obj)
        maybeParens obj = format obj
#endif

instance Pretty NativeLangSym where
    format = text . toString

instance Pretty NativeBlock where
    format (MkBlock params body) = hang
        (text "->" <+> commaSep (elems params)) 2
        (braces . format . elems $ body)

instance Pretty Native where
    format (NError {})  = text "nil"
    format (NBit True)  = text "true"
    format (NBit False) = text "false"
    format (NInt x)     = int x
    format (NNum x)     = float x
    format (NStr x)     = format (toString x)
    format (NSeq x)     = brackets (commaSep $ elems x)
    format (NMap x)     = braces (commaSep $ assocs x)
    format (NBlock x)   = format x

instance Pretty String where
    format = ptext . show

instance Pretty (NativeStr, Native) where
    format (x, y) = sep [format (toString x), text "=>", format y]

sepBy :: (Pretty a) => Doc -> [a] -> Doc
sepBy x = sep . punctuate x . map format

commaSep :: Pretty a => [a] -> Doc
commaSep = sepBy comma
