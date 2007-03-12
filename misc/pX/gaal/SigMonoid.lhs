
instance Monoid Sig
(:+:) :: Sig -> Sig -> Sig
(:+:) = mappend

Params

  s            :+:  s                    = s   -- identity

  :($x)        :+:  :($x is rw)          = :($x is rw)
  :($x)        :+:  :($x is copy)        = :($x is copy)
  :($x is rw)  :+:  :($x is copy)        = fail "incompat" -- ???:($x is copy)

  -- user traits
  :($x is tr1) :+:  :($x is tr2)         = :($x is tr1 is tr2) ? or drop?

  :($x)        :+:  :($x?)               = :($x)
  :($x = 42)   :+:  :($x = 54)           = fail "incompat" -- or :($x?) ???

  :($x)        :+:  :(Int $x)            = :($x)         -- ???
  :(Str $x)    :+:  :(Int $x)            = :(Str|Int $x) -- ???

  :($x)        :+:  :($x where {...})    = :($x)

  :(BinTree $t) :+:  :(BinTree $t (Left $l, Right $r))
                                         = :(BinTree $t (Left $l, Right $r)) 
Signatures

-- mandatories must match

  :($x)        :+:  :($y)                = fail "incompat"
  :($x)        :+:  :($y?)               = :($x, $y?)

  :(:$elk)     :+:  :(:$caribou)         = fail "incompat"

-- slurpy coherence

  :($mand1, *%slurpy1)
               :+:  :($mand1, $opt1?, *%slurpy1)
                                         = :($mand1, $opt1?, *%slurpy1)

  -- slurpies don't have to be treated as mandatory (right?)
  :($mand1, *%slurpy1)
               :+:  :($mand1, $opt1?)
                                         = :($mand1, $opt1?, *%slurpy1)
