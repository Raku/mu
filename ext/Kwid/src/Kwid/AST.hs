{-# OPTIONS -fglasgow-exts #-}

module Kwid.AST where
import Internals

type KwidDocument = [KwidBlock]

data KwidBlock
    = Header { headerLevel :: KwidHeaderLevel, headerPara :: KwidPara }
    | Para { paraPara :: KwidPara }
    | Verbatim KwidText
    | List
    | Comment | Format
    deriving (Show, Eq)

data KwidPhrase
    = Italics KwidPhrase
    | Bold KwidPhrase
    | Code KwidPhrase
    | Plain KwidText
    | Link { linkText :: KwidText, 
             linkResource :: KwidResource, 
             linkSection :: KwidSection }
    | HyperLink { linkText :: KwidText, linkURL :: KwidURL }
    deriving (Show, Eq)

type KwidHeaderLevel = Int -- 1..4
type KwidPara = [KwidPhrase]
type KwidText = String
type KwidURL = String
type KwidResource = String
type KwidSection = String
