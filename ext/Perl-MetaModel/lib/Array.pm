
use v6;

role Array--Perl6;

does Map;
does List;

multi sub push ( @CALLER::_ is rw: *@items ) returns Any;
multi sub unshift ( @CALLER::_ is rw: *@items ) returns Any;

multi sub pop ( @CALLER::_ is rw: ) returns Any;
multi sub shift ( @CALLER::_ is rw: ) returns Any;

multi sub splice ( @CALLER::_ is rw:
		   : Int ?$offset = 0,
                     Int ?$length,
		   *@values) returns List is Lvalue;
