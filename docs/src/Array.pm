
use v6;

role Array;

multi sub pop ( @CALLER::_ ) returns Object;
multi sub push ( @CALLER::_ ) returns Object;
multi sub shift ( @CALLER::_ ) returns Object;
multi sub unshift ( @CALLER::_ ) returns Object;
