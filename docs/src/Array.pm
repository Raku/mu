
use v6;

role Array;

does Map;
does List;

multi sub push ( @CALLER::_ is rw: *@items ) returns Object;
multi sub unshift ( @CALLER::_ is rw: *@items ) returns Object;

multi sub pop ( @CALLER::_ is rw: ) returns Object;
multi sub shift ( @CALLER::_ is rw: ) returns Object;

multi sub splice ( @CALLER::_ is rw:
		   : Int ?$offset = 0,
                     Int ?$length,
		   *@values) returns List is Lvalue;
