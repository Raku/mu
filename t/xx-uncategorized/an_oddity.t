use v6-alpha;

use Test;

=pod
  An oddity that turned up.
  This code yields
    pugs: *** Odd number of elements found where hash expected: VType (mkType "Main")
      at [...] line 1, column 1
  But if the is() is given parens, it "works".
  
=cut


plan 1;

{
    is eval(q{
        my @pairs = (100 => 'lovely');
        @pairs.fmt("%d ==> %s", "\n") 
    }), "100 ==> lovely", '.fmt works with lists of a single pair'
    :todo<feature> :depends<list of single pair>;
}
