multi sub member (*$val)            returns Bool { ?0 }
multi sub member (*$val, *$x, *@xs) returns Bool { ?(($val eq $x) || member($val, |@xs)) }

say "... member";
say member('foo', 1, 'foo', 3, 4, 'bar'); 
say member('baz', 1, 'foo', 3, 4, 'bar');
say member('bar', 1, 'foo', 3, 4, 'bar');           

