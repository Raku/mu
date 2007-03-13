
19:44 < audreyt> property: give two Sig, produce a Sig that, if a Capt
                 can't be bound to it, then it can't be bound to the orig two
19:44 < audreyt> given that property, of course a legit implementation is simply return :(|$)
19:44 < audreyt> but try to do better than that :)

instance Monoid Sig
(:+:) :: Sig -> Sig -> Sig
(:+:) = mappend
sigEmpty = :(|$) -- what is meant by fail below

Params

  -- identity, modulo user traits, constraints and defaults
  s            :+:  s                    = s'

  -- DROP: user traits
  :($x is tr1) :+:  :($x is tr2)         = :($x)        -- drop
  :($x is tr1) :+:  :($x is tr1)         = :($x)        -- drop
  :($x is tr1) :+:  :($x)                = :($x)        -- drop

  -- DROP: default values
  :($x = 42)   :+:  :($x = 42)           = :($x?)       -- drop
  :($x = 42)   :+:  :($x = 54)           = :($x?)       -- drop

  -- DROP: "where" constraints
  :($x)        :+:  :($x where {...})    = :($x)        -- drop

  -- UNIFY: contexts
  :($x)        :+:  :($x is rw)          = :($x is rw)
  :($x)        :+:  :($x is copy)        = :($x is copy)
  :($x is rw)  :+:  :($x is copy)        = :($x is rw)

  -- UNIFY: type constraints
  :($x)        :+:  :(Int $x)            = :($x)
  :(Str $x)    :+:  :(Int $x)            = :(Str|Int $x)

  -- UNIFY: Arity
  :($x)        :+:  :()                  = :($x?),    but because it's easier to implement,
                                           :($?, :$x) which is isomorphic.
  :($x)        :+:  :($x?)               = same as the above case

  -- UNIFY: Unpacking
  :($t)        :+:  :($t (Left $l, Right $r)) = :(BinTree $t) 


Signatures

-- mandatories must match, modulo the other sig allowing the same name as optional

  :($)         :+:  :($x)                = :($, :$x)
  :($x)        :+:  :($y)                = :($, :$x, :$y)
  :($x)        :+:  :($y?)               = :($?, :$x, :$y)
  :($x)        :+:  :(:$x)               = :($?, :$x)
  :($x?)       :+:  :(:$x)               = :($?, :$x)
  :($x)        :+:  :(:$x!)              = :($?, :$x!)

  :($x, $y)    :+:  :($y, $x)            = :($, $, :$x, :$y)
  :($x?, $y?)  :+:  :($y?, $x?)          = :($?, $?, :$x, :$y)

  :($x)        :+:  :($x, $y?)           = :($x, $y?)
  :($x, $y)    :+:  :($x?, $y?)          = :($x?, $y?) -- can get away with it

  :($x, $y)    :+:  :($x?, $z?)          = :($?, $?, :$x, :$y, :$z)
  :($x?)       :+:  :($y?)               = :($?, :$x, :$y)

  :(:$elk)     :+:  :(:$caribou)         = :(:$elk, :$caribou)
  :(:$elk!)    :+:  :(:$caribou!)        = :(:$elk, :$caribou)
  :(:$elk!, :$caribou)
               :+:  :(:$caribou, :$elk!) = :(:$elk, :$caribou)

-- slurpy coherence

  :($mand1, *%slurpy1)
               :+:  :($mand1, $opt1?, *%slurpy1)
                                         = :($mand1, $opt1?, *%slurpy1)

-- slurpies don't have to be treated as mandatory
  :($mand1, *%slurpy1)
               :+:  :($mand1, $opt1?)
                                         = :($mand1, $opt1?, *%slurpy1)



-- vim: set ft=plain :
