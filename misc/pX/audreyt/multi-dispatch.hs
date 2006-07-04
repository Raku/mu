type Boundaries = IntSet
data Variant = MkVariant
    { variantBoundaries :: Boundaries
    , variantParams     :: [Param]
    }

dispatch :: [Variant] -> [Arg] -> Variant
dispatch allVariants args = tryLevel 1 allCandidates
    where
    allCandidates   = filter (isCompatibleWith args . variantParams) allVariants
    maxBoundary     = max allBoundaries
    allBoundaries   = union (map variantBoundaries allCandidates)

    tryLevel count []                   = die "No candidates match at count" count
    tryLevel count [candidate]          = candidate
    tryLevel count candidates
        | count > maxBoundary           = die "Ambiguous candidates" candidates
        | count `elem` allBoundaries    = doLevel count candidates 
        | otherwise                     = tryLevel (count + 1) candidates

    doLevel count candidates = case getWinnersUpToLevel count candidates of
        [] -> die "Ambiguous tiebreaking at count" count
        winners
            | all (count `elem` . variantBoundaries) winners    -- #2
            -> tryLevel (count + 1) winners
            | otherwise
            -> tryLevel (count + 1) candidates

    getWinnersUpToLevel count candidates
        = foldl1 intersection [ getWinnersForLevel l candidates | l <- 1..count ]

    -- A winner is someone who loses to nobody.
    getWinnersForLevel count candidates =
        let arg    = args `idx` count 
            ge x y = greaterOrEqualTo arg (paramIdx x) (paramIdx y)
            paramIdx x  = variantParams x `idx` count
        [ c | c <- candidates, all (\c' -> c `ge` c') candidates ]

    -- Incomparable pair = False,False
    -- Comparable equivs = True,True
    greaterOrEqualTo arg param1 param2 = ...
