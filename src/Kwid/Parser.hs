{-# OPTIONS -fglasgow-exts #-}

module Kwid.Parser where
import Kwid.AST
import Internals
import Lexer
import Rule
import Rule.Error

ruleKwidDocument :: RuleParser KwidDocument
ruleKwidDocument = rule "kwid document" $ do
    many ruleKwidBlock

ruleKwidBlock :: RuleParser KwidBlock
ruleKwidBlock = rule "kwid block" $ choice
    [ ruleKwidHeader
    , ruleKwidPara
    ]

ruleKwidHeader :: RuleParser KwidBlock
ruleKwidHeader = rule "kwid header" $ do
    heading <- many1 (char '=')
    when (length heading > 4) $ do
        fail "Can't have heading deeper than 4th level"
    many1 (char ' ')
    para <- ruleParaLine -- must handle markups!
    return $ Header { headerLevel = length heading, headerPara = para }

ruleParaLine :: RuleParser KwidPara
ruleParaLine = rule "kwid para (one line)" $ do
    text <- ruleTextLine
    return [Plain text] -- a para with only one phrase, a plain one

ruleTextLine :: RuleParser KwidText
ruleTextLine = rule "kwid text (one line)" $ do
    text <- many1 (satisfy (/= '\n'))
    ruleEndOfLine -- this eats the \n but does not return it; also handles eof
    return text

sampleText = "= Foo\ntext\n"
sampleParsed = runParser ruleKwidDocument emptyEnv "file.txt" sampleText
-- file.txt is the label used in the err msg. nothing more

sampleAST :: KwidDocument
sampleAST = case sampleParsed of
    Left err  -> error $ "Parser error: " ++ show err
    Right doc -> doc

sampleASTDumpedAsString :: String
sampleASTDumpedAsString = show sampleAST

