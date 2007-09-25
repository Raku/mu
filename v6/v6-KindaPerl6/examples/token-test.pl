# compile with:
# perl kp6-perl5.pl --perl5rx < examples/token-test.pl | perltidy 

# run with:
# perl -I lib5regex -I lib-kp6-mp6-p5

class X {
  my $zzz;
  
  method y { 42 };  # just testing

  token ws { \s+ };

  token word { \w+ };

  token x { 
    4
    (2)
    [(3)(4)]+ 
    $<xyz> := (.)
    [ $<abc> := (.) ]+

    $<rep> := (.)
    $<rep> := (.)
    
    <?ws>
    <before . >
    <word>
    <after \w >
    { 42 }
    { return 123 }
  };

  $_ := '423434XabRR  xyz';
  X.x();
  say 'match: ', $/.perl;
  say 'result: ', $/.result;
}
