
use v6;

role Math::Basic;

multi sub abs (: Num ?$x = $CALLER::_ )
    returns Num { ... }

multi sub exp (: Num ?$exponent = $CALLER::_, Num +$base )
    returns Num { ... }

multi sub log (: Num ?$x = $CALLER::_, Num +$base where { $base > 0 })
    returns Num { ... }

# mmm, curry!
&log10<> := &log<>.assuming:base(10);

multi sub rand (: Num ?$x = 1, Num +$seed )
    returns Num { ... }

multi sub sign (: Num ?$x = $CALLER::_)
    returns Int { ( defined $x
		    ? ( ($x > 0)
			? 1
			: ($x < 0 ? -1 : 0 ) )
		    : undef ) }

multi sub sqrt (: Num ?$x = $CALLER::_)
    returns Num { exp($x,0.5) }

1;
