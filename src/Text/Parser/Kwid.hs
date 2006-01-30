{-# OPTIONS_GHC -cpp #-}
import Data.Char
import Data.List
import GHC.Exts
import Debug.Trace
import Text.Html

normalize :: String -> IO ()
normalize str = case parse str of
    Fail -> fail "Parsing failed"
    ok   -> putStr (renderText (val ok))

htmlify :: String -> IO ()
htmlify str = case parse str of
    Fail -> fail "Parsing failed"
    ok   -> putStr (prettyHtml (toHtml (val ok)))

eventify :: String -> IO ()
eventify str = case parse str of
    Fail -> fail "Parsing failed"
    ok   -> mapM_ print (renderEvent (val ok))

data Event
    = EventBegin Id
    | EventEnd Id
    | EventText String
    deriving (Ord, Eq)

instance Show Event where
    show (EventBegin x) = "+ " ++ (drop 2 (show x))
    show (EventEnd x)   = "- " ++ (drop 2 (show x))
    show (EventText x)  = "= " ++ show x

class Render a where
    renderText :: a -> String
    renderEvent :: a -> [Event]

instance Render Kwid where
    renderText (MkKwid xs) = concatMap renderText xs
    renderEvent (MkKwid xs) = (EventBegin I_Kwid : concatMap renderEvent xs) ++ [EventEnd I_Kwid]

instance Render Para where
    renderText (MkPara xs) = concatMap renderText xs ++ "\n\n"
    renderText (MkHead i xs) = replicate i '=' ++ (' ':concatMap renderText xs) ++ "\n\n"
    renderEvent (MkPara xs) = (EventBegin I_Para : concatMap renderEvent xs) ++ [EventEnd I_Para]
    renderEvent (MkHead _ xs) = (EventBegin I_Head : concatMap renderEvent xs) ++ [EventEnd I_Head]

instance Render Chunk where
    renderText (Text x) = x
    renderText (Bold xs) = "*" ++ concatMap renderText xs ++ "*"
    renderText (Italic xs) = "/" ++ concatMap renderText xs ++ "/"
    renderEvent (Text x) = [EventText x]
    renderEvent (Bold xs) = (EventBegin I_Bold : concatMap renderEvent xs) ++ [EventEnd I_Bold]
    renderEvent (Italic xs) = (EventBegin I_Italic : concatMap renderEvent xs) ++ [EventEnd I_Italic]

instance HTML Kwid where
    toHtml (MkKwid xs) = body $ toHtmlFromList xs

instance HTML Para where
    toHtml (MkPara xs) = paragraph $ toHtmlFromList xs
    toHtml (MkHead 1 xs) = h1 (toHtmlFromList xs)

instance HTML Chunk where
    toHtml (Text x) = toHtml x
    toHtml (Bold xs) = bold $ toHtmlFromList xs
    toHtml (Italic xs) = emphasize $ toHtmlFromList xs

data Kwid = MkKwid [Para]
    deriving (Show, Ord, Eq)
data Para
    = MkPara [Chunk]
    | MkHead Int [Chunk]
    deriving (Show, Ord, Eq)
data Chunk
    = Text String
    | Bold [Chunk]
    | Italic [Chunk]
    deriving (Show, Ord, Eq)

#ifndef HADDOCK
data Token
    = Star          -- *
    | Slash         -- /
    | Blank         -- \n\n
    | Plain String  -- ...
    | Head1         -- =
    deriving (Show, Ord, Eq)

deToken :: Token -> String
deToken Star = "*"
deToken Slash = "/"
deToken Blank = "\n\n"
deToken Head1 = "="
deToken (Plain x) = x
#endif

#ifndef HADDOCK
data Parser a where
    MkPlain
        :: Id           -- id
        -> (String -> a)-- handler
        -> Parser a
    MkParser
        :: Show b
        => Id           -- id
        -> [Parser b]   -- contains
        -> [Token]      -- begins
        -> [Token]      -- ends
        -> ([b] -> a)   -- handler
        -> Bool         -- reentrant
        -> Parser a
#endif

data Id = I_Kwid | I_Para | I_Italic | I_Bold | I_Plain | I_Head
    deriving (Eq, Ord, Show, Enum)

kwid    = MkParser I_Kwid [head1, para] [] [] MkKwid False
head1   = MkParser I_Head [bld, italic, plain] [Head1] [Blank] (MkHead 1) False
para    = MkParser I_Para [bld, italic, plain] [] [Blank] MkPara False
italic  = MkParser I_Italic [bld, plain] [Slash] [Slash] Italic False
bld     = MkParser I_Bold [italic, plain] [Star] [Star] Bold False
plain   = MkPlain  I_Plain Text

data Top = MkTop { topId :: Id, topEnds :: [Token] }
    deriving (Eq, Ord, Show)

data Response a
    = Cut  { val :: a, rest :: [Token], cutId :: Id }
    | Cont { val :: a, rest :: [Token] }
    | Fail
    deriving (Eq, Ord, Show)

parse :: String -> Response Kwid
parse = parseWith kwid

parseWith :: Parser a -> String -> Response a
parseWith p str = let ?tops = [] in kparse p (klex '\n' str)

kparse :: (?tops :: [Top]) -> Parser a -> [Token] -> Response a
{-
kparse (MkPlain _ f) tokens = chk (reverse ?tops) tokens
    where
    chk tops (Plain p:ts) = let (ps, rs) = chk tops ts in (p++ps, rs)
    chk tops (t:ts) = case find ((t `elem`) . topEnds) tops of
        Just i  -> ("", Cut undefined ts (topId i))
        _       -> (deToken t, "", Cut undefined ts (topId i))
-}
kparse (MkPlain _ f) (Plain s:ts) = checkTops ts (f s)
kparse (MkPlain _ f) []           = Cut (f "") [] I_Kwid
kparse (MkPlain _ _) _            = Fail
kparse (MkParser i contains begins ends handler reentrant) tokens@(t:ts)
    | Just ts' <- if null begins then Just tokens
                    else if t `elem` begins then Just ts
                    else Nothing
    -- check for each "contains" in turn until one fails (or cuts)
    = let ?tops = (MkTop i ends : ?tops) in
    case parseContains ts' of
        [] -> Cont (handler (map val [])) ts'
        rs -> case last rs of
            Cut _ rest i' | i' == i -> Cont (handler (map val rs)) rest
            Cut _ rest i' -> Cut  (handler (map val rs)) rest i'
            Cont _ rest   -> Cont (handler (map val rs)) rest
            _             -> error "impossible"
    where
    parseContains :: (?tops :: [Top]) -> [Token] -> [Response b]
    parseContains tokens = unsafeCoerce# $ case find notFail (map (`kparse` tokens)
        [ c | c <- contains
        , case c of
            MkParser i _ _ _ _ r -> r || all ((/= i) . topId) ?tops
            _                    -> True
        ]
        ) of
        Just r@Cut{}    -> [r]
        Just r          -> let rs = parseContains (rest r) in (r : rs)
        _               -> []
    notFail Fail = False
    notFail _    = True
kparse _ _ = Fail

checkTops :: (?tops :: [Top]) -> [Token] -> a -> Response a
checkTops tokens@(t:ts) x = case find ((t `elem`) . topEnds) (reverse ?tops) of
    Just i  -> Cut x ts (topId i)
    _       -> Cont x tokens
checkTops [] x = Cut x [] I_Kwid

{-
-- List, Para, Bold, Italic
    a parser has a list of things it can contain
    as you're scanning you look for those
    first you look for ending of your ancestors (furtherest first)
    next you look for one of your containing elements
    otherwise it's a grammar error
-}

klex :: Char -> String -> [Token]
klex _ []              = []
klex _ ('*':xs)        = Star:klex '*' xs
klex _ ('/':xs)        = Slash:klex '/' xs
klex _ ('\n':'\n':xs)  = Blank:klex '\n' xs
klex '\n' ('=':' ':xs) = Head1:klex ' ' xs
klex _ (x:xs)          = case klex x xs of
    (Plain t:ts) -> (Plain (x:t):ts)
    ts           -> (Plain [x]:ts)

testString = unlines
    [ "Simple document of /ita *bold* lic/ text."
    , ""
    , "Para two."
    ]
