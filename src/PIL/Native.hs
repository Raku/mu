{-# OPTIONS_GHC -fglasgow-exts -fno-warn-orphans #-}

module PIL.Native (
    evalNativeLang,
    parseNativeLang,
    NativeLangExpression,
    pretty
) where
import PIL.Native.Types
import PIL.Native.Parser
import PIL.Native.Eval
import PIL.Native.Pretty

{-| 

PIL.Native.* 

These modules implement a core set of runtime types along with a 
mini-language for manipulating those types.

This is the lowest level of the ObjectSpace. These are the building blocks 
for all the layers which follow (see Perl6:bjectSpace/docs for more details 
(at least for now)).

PIL.Native.Types 
    - defines our Native types in Haskell

PIL.Native.Coerce 
    - defines the functions for use with the PIL.Native.Types

PIL.Native.Parser
    - implements a parser for the Native runtime mini-language

PIL.Native.Pretty 
    - a pretty printer for the mini-language

PIL.Native.Eval
    - the evaluator for the mini-language

PIL.Native.Prims
    - defines the primative methods of the core types in the mini-language

-}
