{-# OPTIONS -fglasgow-exts #-}

{-

  This needs to be redone as a proper Haskell parser,
  which will be one of my next projects. But so far,
  this works.
  
  The operators are simple prefix operators
  with zero or one argument, except for everything
  that ultimatively goes into @ARGV for the Pugs
  script.
  
-}

module ArgParse
where
import Internals

canonicalArgs :: [String] -> [String]
canonicalArgs(x) = joinDashE (_proc x)

-- clean up later
_unpack(x) = unpackOptions(x)
_sort(x) = sortArgs $ _unpack x
_proc(x) = procArg (foldl (++) [] (_sort x))
    
procArg :: [String] -> [String]
procArg("-I":dir:rest)      = ["-I",dir]                             ++ procArg(rest)
procArg("-V":option:rest)   = ["-V",option]                          ++ procArg(rest)
procArg("-c":rest)          = ["-c"]                                 ++ procArg(rest)
procArg("-d":rest)          = ["-d"]                                 ++ procArg(rest)
procArg("-l":rest)          = ["-e", "# BEGIN { ... } # to be done"] ++ procArg(rest) -- XXX fixme
procArg("-w":rest)          = ["-w"]                                 ++ procArg(rest)
procArg("-e":fragment:rest) = ["-e",fragment]                        ++ procArg(rest)
procArg(xs)                 = xs  -- this must be either the filename or @ARGV

unpackOptions :: [String] -> [String]
unpackOptions xs = foldl (++) [] $ map unpackOption xs

unpackOption :: String -> [String]
unpackOption('-':'c':[])      = ["-c"]
unpackOption('-':'d':[])      = ["-d"]
unpackOption('-':'e':[])      = ["-e"]
unpackOption('-':'h':[])      = ["-h"] -- which direction ?
unpackOption("--help")        = ["-h"] -- verbose -> short ?
unpackOption('-':'l':[])      = ["-l"]
unpackOption('-':'v':[])      = ["-v"]
unpackOption("--version")     = ["-v"]
unpackOption('-':'w':[])      = ["-w"]

-- uncluster clustering options
unpackOption('-':'c':rest)    = unpackOption("-c") ++ unpackOption('-':rest)
unpackOption('-':'d':rest)    = unpackOption("-d") ++ unpackOption('-':rest)
unpackOption('-':'e':frag)    = ["-e",frag]
unpackOption('-':'l':rest)    = unpackOption("-l") ++ unpackOption('-':rest)
unpackOption('-':'w':rest)    = unpackOption("-w") ++ unpackOption('-':rest)
unpackOption('-':'I':[])      = ["-I"]
unpackOption('-':'I':dir)     = ["-I",dir]
unpackOption('-':'V':':':xs)  = ["-V",xs]
unpackOption(a)               = [a]

{-

  sortArgs enforces a canonical order of command line switches.
  Currently this is:

    (-h -v -V) (-I) (-d) (-w) (-c) (-l -0 -e other)

  This makes pattern matching more convenient

-}

-- Schwartzian transform, overkill but nice ;))
sortArgs :: [String] -> [[String]]
sortArgs args = _unpackArgs $ _sortArgs $ _packArgs $ _gatherArgs args
  where
    _packArgs args    = map (\ a -> (argRank a,a)) args
    _sortArgs args    = sortBy (\ (a,_) (b,_) -> compare a b) args
    _unpackArgs args  = map (\ (_,b) -> b ) args

argRank :: [String] -> Int
argRank(("-h"):_)         = -1
argRank(("-v"):_)         = -1
argRank(("-V"):_:[])      = -1
argRank(("-V"):[])        = -1
argRank(("-I"):_)         = 0
argRank(("-d"):_)         = 1
argRank(("-w"):_)         = 2
argRank(("-c"):_)         = 3
argRank(("-l"):_)         = 100  -- translated into Perl code (later)
argRank(("-0"):_)         = 100  -- translated into Perl code (later)
argRank(("-e"):_)         = 100  -- translated into Perl code
argRank(_)                = 100  -- filename or @ARGV or whatever

-- Gather switches and their arguments:
_gatherArgs :: [String] -> [[String]]
_gatherArgs([]) = []
_gatherArgs("-e":frag:rest) = [["-e",frag]] ++ _gatherArgs(rest)
_gatherArgs("-V":item:rest) = [["-V",item]] ++ _gatherArgs(rest)
_gatherArgs("-I":dir:rest)  = [["-I",dir]] ++ _gatherArgs(rest)
-- _gatherArgs("-l":sep:rest)  = [["-l",sep]] ++ _gatherArgs(rest)
-- _gatherArgs("-0":sep:rest)  = [["-0",sep]] ++ _gatherArgs(rest)
_gatherArgs(('-':x):xs) = [['-':x]] ++ _gatherArgs(xs)
_gatherArgs(x)              = [x] -- gobbles up the rest in one lump

{- collect "-e" switches together -}
joinDashE :: [String] -> [String]
joinDashE [] = []
joinDashE ("-e":a:"-e":b:args) = do
                                   joinDashE (("-e"):combined:args)
                                 where
                                   combined = a++"\n"++b
joinDashE (x:xs) =  [ x ] ++ joinDashE xs

