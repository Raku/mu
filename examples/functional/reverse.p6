multi sub reverse ()          { ()                 }
multi sub reverse (*$x, *@xs) { (reverse(@xs), $x) }

say "... reverse";
my @result = reverse(1, 'foo', 3, 4, 'bar');
say join ", ", @result; 

my @result = reverse('foo');            
say join ", ", @result; 

# my @result = reverse();
# say join ", ", @result; 

