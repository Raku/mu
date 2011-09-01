multi sub member (*$val)            returns Bool { ?0 }
multi sub member (*$val, *$x, *@xs) returns Bool { ?(($val eq $x) || member($val, |@xs)) }

say "... member";
say member('foo', 1, 'foo', 3, 4, 'bar'); 
say member('baz', 1, 'foo', 3, 4, 'bar');
say member('bar', 1, 'foo', 3, 4, 'bar');           



multi sub unique ()          { () }
multi sub unique (*$x, *@xs) { member($x, |@xs) ?? unique(|@xs) !! ($x, unique(|@xs)) }

say "... unique";
my @result = unique('foo', 5, 4, 3, 3, 3, 3, 1, 'foo', 3, 4, 'bar');
say join ", ", @result;

@result = unique(1, 2, 3, 4, 1, 2, 3, 2, 2, 1, 1, 1, 1, 2, 4, 1, 1);
say join ", ", @result;

@result = unique(1, 1, 1, 1, 1, 1, 1, 1);
say join ", ", @result;

@result = unique('foo');
say join ", ", @result;

# my @result = unique(); 
# say join ", ", @result;

