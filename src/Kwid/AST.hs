module Kwid.AST where
import Internals

type KwidDocument = [KwidBlock]

type KwidHeaderLevel = Integer -- 1..4
type KwidPara = [KwidPhrase]

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

type KwidText = String
type KwidURL = String
type KwidResource = String
type KwidSection = String

data KwidBlock
    = Header { headerLevel :: KwidHeaderLevel, headerPara :: KwidPara }
    | Para { paraPara :: KwidPara }
    | Verbatim KwidText
    | List
    | Comment | Format
    deriving (Show, Eq)
