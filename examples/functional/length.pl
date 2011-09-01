

multi sub length ()          returns Int { 0               }
multi sub length (*$x, *@xs) returns Int { 1 + length(|@xs) }

sub length2(*@x) returns Int{
    multi sub iter(*$curr) returns Int{ $curr }
    multi sub iter(*$curr, *$x, *@xs) returns Int{
        iter($curr + 1, |@xs);
    }
    iter(0, @x);
}

say "... length";
say length(1, 'foo', 3, 4, 'bar'); 
say length('foo');            
say length2(1, 'foo', 3, 4, 'bar'); 
say length2('foo');            
say length();
