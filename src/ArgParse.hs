{-# OPTIONS -fglasgow-exts #-}

{-

  This needs to be redone as a proper Haskell parser,
  which will be one of my next projects. But so far,
  this works.
  
  The operators are simple prefix operators
  with zero or one argument, except for everything
  that ultimatively goes into @ARGS for the Pugs
  script.
  
-}

module ArgParse (canonicalArgs)
where
import Internals

canonicalArgs :: [String] -> [String]
canonicalArgs x = concatMap procArg
                . joinDashE
                . sortBy compareArgs
                . gatherArgs
                . unpackOptions
                $ x

data Arg = File String | Switch Char | Opt String String

procArg :: Arg -> [String]
procArg (Opt name arg)  = [name, arg]
procArg (File name)     = [name]
procArg (Switch name)   = ['-':name:[]]

unpackOptions :: [String] -> [String]
unpackOptions [] = []
unpackOptions ("--":opts) = opts
unpackOptions (('-':opt):rest) = unpackOption opt ++ unpackOptions rest
unpackOptions (filename:rest) = filename : unpackOptions rest

unpackOption :: String -> [String]
unpackOption "" = [] -- base case for composing
unpackOption opt
    | Just short <- lookup ('-':opt) longOptions = [short]
    | head opt `elem` composable = ('-':head opt:[]) : unpackOption (tail opt)
    | Just (prefix, param) <- prefixOpt opt = ['-':prefix, param]
    | otherwise = ['-':opt]

longOptions = [("--help", "-h"), ("--version", "-v")]
composable = "cdlw"
withParam = ["e", "C", "I", "V:"]
prefixOpt opt = msum $ map (findArg opt) withParam
findArg arg prefix = do
    param <- afterPrefix prefix arg
    return (prefix, param)

{-

  compareArgs enforces a canonical order of command line switches.
  Currently this is:

    (-h -v -V) (-I) (-d) (-w) (-c) (-C) (--external) (-l -0 -e other)

  This makes pattern matching more convenient

-}

compareArgs a b = compare (argRank a) (argRank b)

argRank :: Arg -> Int
argRank(Switch 'h')         = -1
argRank(Switch 'v')         = -1
argRank(Opt "-V" _)         = -1
argRank(Switch 'V')         = -1
argRank(Opt "-I" _)         = 0
argRank(Switch 'd')         = 1
argRank(Switch 'w')         = 2
argRank(Switch 'c')         = 3
argRank(Opt "-C" _)         = 4
argRank(Opt "--external" _) = 5
argRank(Switch 'l')         = 100  -- translated into Perl code (later)
argRank(Switch '0')         = 100  -- translated into Perl code (later)
argRank(Opt "-e" _)         = 100  -- translated into Perl code
argRank(_)                  = 100  -- filename or @ARGS or whatever

-- Gather switches and their arguments:
-- should be abstracted together into some generative code
-- (but I don't know yet how to write such code in Haskell)
gatherArgs :: [String] -> [Arg]
gatherArgs([]) = []
gatherArgs("-e":frag:rest)        = [Opt "-e" frag] ++ gatherArgs(rest)
gatherArgs("--external":mod:rest) = [Opt "--external" mod] ++ gatherArgs(rest)
gatherArgs("-I":dir:rest)         = [Opt "-I" dir] ++ gatherArgs(rest)
gatherArgs("-C":backend:rest)     = [Opt "-C" backend] ++ gatherArgs(rest)
gatherArgs("-V":item:rest)        = [Opt "-V" item] ++ gatherArgs(rest)
-- _gatherArgs("-l":sep:rest)      = [["-l",sep]] ++ _gatherArgs(rest) -- XXX implement later
-- _gatherArgs("-0":sep:rest)      = [["-0",sep]] ++ _gatherArgs(rest) -- XXX implement later
gatherArgs(('-':x):xs)            = [Switch (head x)] ++ gatherArgs(xs)
gatherArgs(x:xs)                  = [File x] ++ gatherArgs(xs)

{- collect "-e" switches together -}
joinDashE :: [Arg] -> [Arg]
joinDashE [] = []
joinDashE ((Opt "-e" a):(Opt "-e" b):args) =
    joinDashE (Opt "-e" combined:args)
    where
    combined = a++"\n"++b
joinDashE (x:xs) =  [ x ] ++ joinDashE xs

