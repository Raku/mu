module Kwid.Compile where
import Kwid.AST
import Kwid.Parser
import Internals
import Text.Html

-- kwidToBytecode :: String -> String

kwidToHtml :: String -> String
kwidToHtml kwidString htmlString = renderHtml htmlAST
    where
    htmlAST = docToHtml kwidAST
    kwidAST = parseKwidString kwidString

parseKwidString :: String -> KwidDocument
parseKwidString str = case runParser ruleKwidDocument emptyEnv "" str of
    Left err  -> error $ "Parser error: " ++ show err
    Right doc -> doc

docToHtml (Verbatim text) = pre $ stringToHtml text
docToHtml (Para para) = paragraph $ paraToHtml para
docToHtml (Header lvl para) = lvlTag $ paraToHtml para
    where
    lvlTag = case lvl of
        1 -> h1
        2 -> h2
        3 -> h3
        4 -> h4

paraToHtml phrases = concatHtml $ map phraseToHtml phrases

phraseToHtml (Plain text) = stringToHtml text
phraseToHtml (Italics phrase) = italics $ phrasetoHtml phrase
phraseToHtml (Bold phrase) = bold $ phrasetoHtml phrase
phraseToHtml (HyperLink txt url) = anchor ! [href url] << stringToHtml txt

{-
html things are defined in Text.Html
similarily if you want a bytecode, define your Text.KwidBytecode
that provides bold and other primitives.
-}
