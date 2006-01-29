{-# OPTIONS_GHC -fglasgow-exts #-}

{-

  This needs to be redone as a proper Haskell parser,
  which will be one of my next projects. But so far,
  this works.

  The operators are simple prefix operators
  with zero or one argument, except for everything
  that ultimatively goes into @ARGS for the Pugs
  script.

  If you change anything here, make sure all tests under
  t/pugsrun/ still pass. Otherwise you might break building
  for everybody, once you commit.

-}

-- | Command line argument parser for pugs.
module Pugs.Run.Args (
    canonicalArgs,
    gatherArgs,
    unpackOptions,
) where
import Pugs.Internals

{- | 
  Convert command line arguments into canonical form for 
  'Pugs.Run.runWithArgs'.  The switch ordering is defined
  by compareArgs and is currently:

  > (-h -v -V) (-I) (-d) (-w) (-c) (-C) (--external) (-M) (-n -p) (-l -0 -e other)

  Args -M, -n and -p are converted to -e scripts by desugarDashE.
-}
canonicalArgs :: [String] -> [String]
canonicalArgs x = concatMap procArg
                . concatDashE
                . desugarDashE
                . sortBy compareArgs
                . gatherArgs
                . unpackOptions
                $ x

concatDashE :: [Arg] -> [Arg]
concatDashE (Opt "-e" e:xs) = (Opt "-e" $ concat (intersperse "\n" (e:map optArg es))) : rest
    where
    (es, rest)          = partition isOptE xs
    isOptE (Opt "-e" _) = True
    isOptE _            = False
concatDashE (x:xs) = (x:concatDashE xs)
concatDashE xs = xs

data Arg
    = File !String
    | Switch !Char
    | Opt { optFlag :: !String, optArg :: !String }
    deriving Show

procArg :: Arg -> [String]
procArg (Opt name arg)  = [name, arg]
procArg (File name)     = [name]
procArg (Switch name)   = ['-':name:[]]

unpackOptions :: [String] -> [String]
unpackOptions []                = []
unpackOptions (("-"):rest)      = ("-":unpackOptions rest)
unpackOptions opts@("--":_)     = opts
unpackOptions (('-':opt):arg:rest)
    | takesArg opt              = unpackOption opt ++ (arg:unpackOptions rest)
unpackOptions (('-':opt):rest)  = unpackOption opt ++ unpackOptions rest
unpackOptions opts@[filename]   = opts
unpackOptions (filename:rest)   = filename : "--" : rest

takesArg :: String -> Bool
takesArg xs     | xs `elem` withParam   = True
takesArg (x:xs) | x `elem` composable   = takesArg xs
takesArg _                              = False

unpackOption :: String -> [String]
unpackOption "" = [] -- base case for composing
unpackOption opt
    | Just short <- lookup ('-':opt) longOptions = [short]
    | head opt `elem` composable = ['-', head opt] : unpackOption (tail opt)
    | Just (prefix, param) <- prefixOpt opt = ['-':prefix, param]
    | otherwise = ['-':opt]

-- | List of options with long and sort variants, as tupples of long, short (with the dashes).
longOptions :: [(String, String)]
longOptions = [("--help", "-h"), ("--version", "-v")]

-- | List of options that can have their argument just after, with no space.
composable :: [Char]
composable = "cdlnpw"

-- | List of options that can take arguments
withParam :: [String]
withParam = words "e C B I M V:"

prefixOpt :: [Char] -> Maybe (String, String)
prefixOpt opt = msum $ map (findArg opt) withParam

findArg :: Eq a => [a] -> [a] -> Maybe ([a], [a])
findArg arg prefix = do
    param <- afterPrefix prefix arg
    guard (not (null param))
    return (prefix, param)

{-
  Enforce a canonical order of command line switches.  Currently this is:

  > (-h -v -V) (-I) (-d) (-w) (-c) (-C) (--external) (-M) (-n -p) (-l -0 -e other)

  This makes pattern matching more convenient

  Backwards incompatible changes:

   *  -p and -n autochomp.

   *  -p uses say() instead of print()
-}

compareArgs :: Arg -> Arg -> Ordering
compareArgs a b = compare (argRank a) (argRank b)

argRank :: Arg -> Int
argRank (Switch 'h')         = -1
argRank (Switch 'v')         = -1
argRank (Opt "-V:" _)        = -1
argRank (Switch 'V')         = -1
argRank (Opt "-I" _)         = 0
argRank (Switch 'd')         = 1
argRank (Switch 'w')         = 2
argRank (Switch 'c')         = 3
argRank (Opt "-C" _)         = 4
argRank (Opt "-B" _)         = 4
argRank (Opt "--external" _) = 5
argRank (Opt "-M" _)         = 98
argRank (Switch 'n')         = 99   -- translated into Perl code (later)
argRank (Switch 'p')         = 99   -- translated into Perl code (later)
argRank (Switch 'l')         = 100  -- translated into Perl code (later)
argRank (Switch '0')         = 100  -- translated into Perl code (later)
argRank (Opt "-e" _)         = 100  -- translated into Perl code
argRank _                    = 100  -- filename or @ARGS or whatever

gatherArgs :: [String] -> [Arg]
gatherArgs [] = []
-- XXX implement BEGIN block later
gatherArgs ("-l":rest)             = gatherArgs("-e":"# BEGIN { ... } # to be done":rest)
gatherArgs ("-e":frag:rest)        = [Opt "-e" frag] ++ gatherArgs(rest)
gatherArgs ("--external":mod:rest) = [Opt "--external" mod] ++ gatherArgs(rest)
gatherArgs ("-I":dir:rest)         = [Opt "-I" dir] ++ gatherArgs(rest)
gatherArgs ("-M":mod:rest)         = [Opt "-M" mod] ++ gatherArgs(rest)
gatherArgs ("-C":backend:rest)     = [Opt "-C" backend] ++ gatherArgs(rest)
gatherArgs ("-B":backend:rest)     = [Opt "-B" backend] ++ gatherArgs(rest)
gatherArgs ("-V:":item:rest)       = [Opt "-V:" item] ++ gatherArgs(rest)
gatherArgs (('-':[]):xs)           = [File "-"] ++ gatherArgs(xs)
gatherArgs (("--"):rest)           = [File x | x <- rest]
gatherArgs (('-':x:[]):xs)         = [Switch x] ++ gatherArgs(xs)
gatherArgs (x:xs)                  = [File x] ++ gatherArgs(xs)

{- collect "-e" switches together,
   handle transformation of "-M", "-n"
   and "-p" into "-e" fragments
-}
desugarDashE :: [Arg] -> [Arg]
desugarDashE [] = []
desugarDashE ((Switch 'p'):args) = desugarDashE $
    (Opt "-e" "env $_; while ($_ = =<>) { $_ = chomp($_);" : args) ++ [Opt "-e" "; say $_; }"]
desugarDashE ((Switch 'n'):args) = desugarDashE $
    (Opt "-e" "env $_; while ($_ = =<>) { $_ = chomp($_);" : args) ++ [Opt "-e" "}"]

-- -E is like -e, but not accessible as a normal parameter and used only
-- internally:
--   "-e foo bar.p6" executes "foo" with @*ARGS[0] eq "bar.p6",
--   "-E foo bar.p6" executes "foo" and then bar.p6.
desugarDashE ((Opt "-M" mod):args) = desugarDashE ((Opt "-E" (";use " ++ mod ++ ";\n")):args)

-- Preserve the curious Perl5 behaviour:
--   perl -e 'print CGI->VERSION' -MCGI     # works
--   perl print_cgi.pl -MCGI                # fails
desugarDashE (x@(Opt "-e" _):y@(Opt "-E" _):args) = desugarDashE (y:x:args)
desugarDashE ((Opt "-E" a):y@(Opt "-e" _):args) = desugarDashE ((Opt "-e" a):y:args)
desugarDashE (x:xs) = (x:desugarDashE xs)
