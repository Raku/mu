# compile with:
# perl kp6-perl5.pl -do MetaClass EmitPerl5Regex < examples/token-test.pl | perltidy 

# run with:
# perl -I lib5regex -I lib 

class X {
  my $zzz;
  
  method y { 42 };  # just testing
  token x { 
    4
    (2)
    [(3)(4)]+ 
    $<xyz> := (.)
    [ $<abc> := (.) ]+

    $<rep> := (.)
    $<rep> := (.)

  };   

  $_ := '423434XabRR';
  X.x();
  say 'result: ', $/.perl;
}
