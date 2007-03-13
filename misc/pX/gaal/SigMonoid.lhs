
19:44 < audreyt> property: give two Sig, produce a Sig that, if a Capt
                 can't be bound to it, then it can't be bound to the orig two
19:44 < audreyt> given that property, of course a legit implementation is simply return :(|$)
19:44 < audreyt> but try to do better than that :)

instance Monoid Sig
(:+:) :: Sig -> Sig -> Sig
(:+:) = mappend
sigEmpty = :(|$) -- what is meant by fail below

Params

  s            :+:  s                    = s   -- identity

  -- contexts
  :($x)        :+:  :($x is rw)          = :($x is rw)
  :($x)        :+:  :($x is copy)        = :($x is copy)
  :($x is rw)  :+:  :($x is copy)        = :($x is rw)

  -- user traits
  :($x is tr1) :+:  :($x is tr2)         = :($x)    -- drop
  :($x is tr1) :+:  :($x is tr1)         = :($x)    -- eq rule
  :($x is tr1) :+:  :($x)                = :($x)    -- drop

  :($x)        :+:  :($x?)               = :($x?)   -- see below on matching with optionals
  :($x = 42)   :+:  :($x = 42)           = :($x = 42) -- eq rule
  :($x = 42)   :+:  :($x = 54)           = :($x?)

  :($x)        :+:  :(Int $x)            = :($x)
  :(Str $x)    :+:  :(Int $x)            = :(Str|Int $x)

  :($x)        :+:  :($x where {...})    = :($x)

  :($t)        :+:  :(BinTree $t (Left $l, Right $r))
                                         = :(BinTree $t (Left $l, Right $r)) 
Signatures

-- mandatories must match, modulo the other sig allowing the same name as optional

  :($)         :+:  :($x)                = :($x)   -- name-fixing positionals
  :($x)        :+:  :($y)                = fail "incompat"
  :($x)        :+:  :($y?)               = fail "incompat"
  :($x)        :+:  :(:$x)               = :($x)
  :($x?)       :+:  :(:$x?)              = :($x?)  -- unify named to positional (optional both)

  :($x)        :+:  :($x, $y?)           = :($x)       -- $y cannot appear
  :($x, $y)    :+:  :($x?, $y?)          = :($x?, $y?) -- can get away with it
  :($x, $y)    :+:  :($x?, $z?)          = :($x?)

  :(:$elk)     :+:  :(:$caribou)         = fail "incompat"
  :(:$elk, :$caribou?)
               :+:  :(:$caribou, :$elk?) = :(:$elk?, :$caribou?)

-- slurpy coherence

  :($mand1, *%slurpy1)
               :+:  :($mand1, $opt1?, *%slurpy1)
                                         = :($mand1, $opt1?, *%slurpy1)

-- slurpies don't have to be treated as mandatory
  :($mand1, *%slurpy1)
               :+:  :($mand1, $opt1?)
                                         = :($mand1, $opt1?, *%slurpy1)



-- vim: set ft=plain :
