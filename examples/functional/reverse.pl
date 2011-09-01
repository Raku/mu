# Please remember to update t/examples/examples.t and rename
# examples/output/functional/reverse if you rename/move this file.

multi sub rev ()          { ()                 }
multi sub rev (*$x, *@xs) { (rev(|@xs), $x) }

say "... reverse";
my @result = rev(1, 'foo', 3, 4, 'bar');
say join ", ", @result; 

@result = rev('foo');
say join ", ", @result; 

# my @result = reverse();
# say join ", ", @result; 

