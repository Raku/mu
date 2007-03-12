
instance Monoid Sig
(:+:) :: Sig -> Sig -> Sig
(:+:) = mappend

Params

  s            :+:  s                    = s   -- identity

  -- contexts
  :($x)        :+:  :($x is rw)          = :($x is rw)
  :($x)        :+:  :($x is copy)        = :($x is copy)
  :($x is rw)  :+:  :($x is copy)        = :($x is rw)

  -- user traits
  :($x is tr1) :+:  :($x is tr2)         = :($x)    -- drop

  :($x)        :+:  :($x?)               = :($x?)
  :($x = 42)   :+:  :($x = 54)           = :($x?)

  :($x)        :+:  :(Int $x)            = :($x)
  :(Str $x)    :+:  :(Int $x)            = :(Str|Int $x)

  :($x)        :+:  :($x where {...})    = :($x)

  :($t)        :+:  :(BinTree $t (Left $l, Right $r))
                                         = :(BinTree $t (Left $l, Right $r)) 
Signatures

-- mandatories must match

  :($)         :+:  :($x)                = :($x)   -- name-fixing positionals
  :($x)        :+:  :($y)                = fail "incompat"
  :($x)        :+:  :($y?)               = fail "incompat"

  :(:$elk)     :+:  :(:$caribou)         = fail "incompat"

-- slurpy coherence

  :($mand1, *%slurpy1)
               :+:  :($mand1, $opt1?, *%slurpy1)
                                         = :($mand1, $opt1?, *%slurpy1)

-- slurpies don't have to be treated as mandatory
  :($mand1, *%slurpy1)
               :+:  :($mand1, $opt1?)
                                         = :($mand1, $opt1?, *%slurpy1)
