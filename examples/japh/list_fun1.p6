use v6;
# based loosely on http://www.perlmonks.org/?node_id=199499 
# domm@zsi.at

for([<j 1 t 3 l c 7>],[<u a h P 5 k>],[<s n e e h e>],
[<t o r r a r>])->$J,$A,$P,$H{for zip(@$J,@$A,@$P,@$H)
->$j,$a,$p,$h{print map{$_~~/\d/??" "!!$_}$j,$a,$p,$h}
}.say

