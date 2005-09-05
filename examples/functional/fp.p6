#!perl6
use v6;

# This test demonstrates the use of perl 6 multi-subs to 
# mimic the pattern matching style of functional programming
# languages. 

# Please remember to update t/examples/examples.t and rename
# examples/output/functional/fp if you rename/move this file.

multi sub length ()          returns Int { 0               }
multi sub length (*$x, *@xs) returns Int { 1 + length(@xs) }

sub length2(*@x) returns Int{
    multi sub iter(*$curr) returns Int{ $curr }
    multi sub iter(*$curr, *$x, *@xs) returns Int{
	iter($curr + 1, @xs);
    }
    iter(0, @x);
}

say "... length";
say length(1, 'foo', 3, 4, 'bar'); 
say length('foo');            
say length2(1, 'foo', 3, 4, 'bar'); 
say length2('foo');            
# say length();

multi sub sum ()          returns Int { 0             }
multi sub sum (*$x, *@xs) returns Int { $x + sum(@xs) }

say "... sum";
say sum(1 .. 10);
say sum(1 .. 5);
say sum(2, 2, 2, 2);

sub is_even (Int $val) returns Bool { ($val <= 0) ?? 1 :: is_odd($val - 1)  }
sub is_odd  (Int $val) returns Bool { ($val <= 0) ?? 0 :: is_even($val - 1) }

say "... mutually recursive even and odd predicates";
say is_even(4);
say is_odd(4);
say is_even(5);
say is_odd(5);

multi sub reverse ()          { ()                 }
multi sub reverse (*$x, *@xs) { (reverse(@xs), $x) }

say "... reverse";
my @result = reverse(1, 'foo', 3, 4, 'bar');
say join ", ", @result; 

my @result = reverse('foo');            
say join ", ", @result; 

# my @result = reverse();
# say join ", ", @result; 

multi sub member (*$val)            returns Bool { 0 }
multi sub member (*$val, *$x, *@xs) returns Bool { ($val eq $x) || member($val, @xs) }

say "... member";
say member('foo', 1, 'foo', 3, 4, 'bar'); 
say member('baz', 1, 'foo', 3, 4, 'bar');
say member('bar', 1, 'foo', 3, 4, 'bar');           

multi sub unique ()          { () }
multi sub unique (*$x, *@xs) { member($x, @xs) ?? unique(@xs) :: ($x, unique(@xs)) }

say "... unique";
my @result = unique('foo', 5, 4, 3, 3, 3, 3, 1, 'foo', 3, 4, 'bar'); 
say join ", ", @result;

my @result = unique(1, 2, 3, 4, 1, 2, 3, 2, 2, 1, 1, 1, 1, 2, 4, 1, 1); 
say join ", ", @result;

my @result = unique(1, 1, 1, 1, 1, 1, 1, 1); 
say join ", ", @result;

my @result = unique('foo'); 
say join ", ", @result;

# my @result = unique(); 
# say join ", ", @result;

