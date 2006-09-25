
use v6-alpha;

# Grammar::Python
# experimental Perl6 implementation of the 'Python' syntax 
# author: Flavio S. Glock - fglock@gmail.com

# See also: Parrot languages/python/python.bnf

grammar Pugs::Grammar::Python;

token statement {
    # TODO
    (  <?ident> \N*  )
    { 
      return $$[0] 
    }
}

token line {
    ^^ $<indent> := [ <?ws>? ]   <statement>   [ \n | $ ]
    { 
      return { 
        indent => $<indent>.chars,  # TODO expand tabs
        line   => $$<statement>, 
      }
    }
}

token parse {
    <line>*
    {             
      use v5;
        my @tree;
        my @leaves;
        my @a = map { $$_ } @{ $::_V6_MATCH_->{'line'} };
        while (@a) {
          while (@leaves) {
            if ( $a[0]{indent} > $leaves[-1]{indent} ) {
              push @{ $leaves[-1]{suite} }, $a[0];
              push @leaves, shift @a;
              next;
            }
            pop @leaves;
          }
          push @tree,   $a[0];
          push @leaves, shift @a;
        }
        return { lines => \@tree, };
      use v6;
    }
}
