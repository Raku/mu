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

module Pugs.Run.Args (canonicalArgs
, gatherArgs
, unpackOptions

)
where
import Pugs.Internals

canonicalArgs :: [String] -> [String]
canonicalArgs x = concatMap procArg
                . joinDashE
                . sortBy compareArgs
                . gatherArgs
                . unpackOptions
                $ x

data Arg = File String | Switch Char | Opt String String
  deriving Show

procArg :: Arg -> [String]
procArg (Opt name arg)  = [name, arg]
procArg (File name)     = [name]
procArg (Switch name)   = ['-':name:[]]

unpackOptions :: [String] -> [String]
unpackOptions [] = []
unpackOptions (('-':[]):rest)  = ["-"] ++ unpackOptions rest
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
composable = "cdlnpw"
withParam = ["e", "C", "I", "M", "V:"]
prefixOpt opt = msum $ map (findArg opt) withParam
findArg arg prefix = do
    param <- afterPrefix prefix arg
    guard (not (null param))
    return (prefix, param)

{-

  compareArgs enforces a canonical order of command line switches.
  Currently this is:

    (-h -v -V) (-I) (-d) (-w) (-c) (-C) (--external) (-M) (-n -p) (-l -0 -e other)

  This makes pattern matching more convenient

  Backwards incompatible changes:
    -p and -n autochomp.
    -p uses say() instead of print()

-}

compareArgs a b = compare (argRank a) (argRank b)

argRank :: Arg -> Int
argRank(Switch 'h')         = -1
argRank(Switch 'v')         = -1
argRank(Opt "-V:" _)        = -1
argRank(Switch 'V')         = -1
argRank(Opt "-I" _)         = 0
argRank(Switch 'd')         = 1
argRank(Switch 'w')         = 2
argRank(Switch 'c')         = 3
argRank(Opt "-C" _)         = 4
argRank(Opt "--external" _) = 5
argRank(Opt "-M" _)         = 98
argRank(Switch 'n')         = 99   -- translated into Perl code (later)
argRank(Switch 'p')         = 99   -- translated into Perl code (later)
argRank(Switch 'l')         = 100  -- translated into Perl code (later)
argRank(Switch '0')         = 100  -- translated into Perl code (later)
argRank(Opt "-e" _)         = 100  -- translated into Perl code
argRank(_)                  = 100  -- filename or @ARGS or whatever

gatherArgs :: [String] -> [Arg]
gatherArgs([]) = []
gatherArgs("-l":rest)             = gatherArgs("-e":"# BEGIN { ... } # to be done":rest) -- XXX implement BEGIN block later
gatherArgs("-e":frag:rest)        = [Opt "-e" frag] ++ gatherArgs(rest)
gatherArgs("--external":mod:rest) = [Opt "--external" mod] ++ gatherArgs(rest)
gatherArgs("-I":dir:rest)         = [Opt "-I" dir] ++ gatherArgs(rest)
gatherArgs("-M":mod:rest)         = [Opt "-M" mod] ++ gatherArgs(rest)
gatherArgs("-C":backend:rest)     = [Opt "-C" backend] ++ gatherArgs(rest)
gatherArgs("-V:":item:rest)       = [Opt "-V:" item] ++ gatherArgs(rest)
gatherArgs(('-':[]):xs)           = [File "-"] ++ gatherArgs(xs)
gatherArgs(('-':x):xs)            = [Switch (head x)] ++ gatherArgs(xs)
gatherArgs(x:xs)                  = [File x] ++ gatherArgs(xs)

{- collect "-e" switches together,
   handle transformation of "-M", "-n"
   and "-p" into "-e" fragments
-}
joinDashE :: [Arg] -> [Arg]
joinDashE [] = []
joinDashE ((Switch 'p'):args) = joinDashE ((Opt "-e" "while ($_ = =<>) { chomp $_;"):script++[(Opt "-e" "; say $_; }")]++rest)
                                 where
                                   (script,rest) = partition isDashE args
                                   isDashE (Opt "-e" _) = True
                                   isDashE (_) = False
joinDashE ((Switch 'n'):args) = joinDashE ((Opt "-e" "while ($_ = =<>) { chomp $_;"):script++[(Opt "-e" "}")]++rest)
                                 where
                                   (script,rest) = partition isDashE args
                                   isDashE (Opt "-e" _) = True
                                   isDashE (_) = False

joinDashE ((Opt "-M" mod):args) = joinDashE ((Opt "-e" ("require " ++ mod ++ ";\n")):args)

joinDashE ((Opt "-e" a):(Opt "-e" b):args) =
    joinDashE (Opt "-e" combined:args)
    where
    combined = a++"\n"++b
joinDashE (x:xs) =  [ x ] ++ joinDashE xs
